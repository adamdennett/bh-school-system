"""extract_foi_allocations.py - turn the FOI allocation maps into points

Brighton & Hove released eight maps under FOI 26370589 (29 October 2026,
request "Evaluation of Secondary School Admission Arrangements Changes",
https://www.whatdotheyknow.com/request/evaluation_of_secondary_school_a).
Each shows, for one community school, where the children allocated to it
on National Offer Day live, coloured by the preference rank that got them
the place. The council's covering letter calls them "the illustrations of
the impact of the policy on National Offer Day".

The maps are pictures, but they are georeferenced pictures: the British
National Grid tick labels are live text in the PDF, so the page-to-grid
transform is fitted rather than guessed, and lands inside a metre. The
dots are saturated colours on a pastel basemap, so they come out with a
colour threshold and connected components.

The one thing that cannot be recovered is dots that overlap. Where several
children live close together the symbols merge into one blob, and all that
survives is the blob's centroid and its area. Area divided by the area of
a lone dot estimates how many children it holds; est_dots carries that and
`merged` flags it. On the busiest page a third of the children sit inside
a merged blob, so any count taken from blob rows alone is too low.

This is the only Python in the repository. Its output CSV is committed, so
nothing in the R pipeline needs Python to run - this is here so the
extraction can be audited and repeated, not because it runs routinely.

    python R/extract_foi_allocations.py "<path to FOI 26370589 Allocations.pdf>"

Writes data-raw/foi_allocations_points.csv. Read-only on the PDF.
"""
from __future__ import annotations

import csv
import os
import re
import sys

import cv2
import numpy as np
from PIL import Image
from pypdf import PdfReader

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
OUT = os.path.join(ROOT, "data-raw", "foi_allocations_points.csv")

# The page is drawn inside `0.75 0 0 -0.75 0 PAGE_H cm`, so image placements
# are in a flipped, 0.75-scaled space while text sits in page space.
FLIP_SCALE = 0.75


def page_font(page):
    """The CID font's code-to-character map, for decoding the TJ arrays."""
    for key in page["/Resources"]["/Font"]:
        f = page["/Resources"]["/Font"][key].get_object()
        if "/ToUnicode" in f:
            cm = f["/ToUnicode"].get_data().decode("latin-1")
            return {a.lower(): chr(int(b[:4], 16)) for a, b in
                    re.findall(r"<([0-9a-fA-F]{4})>\s*<([0-9a-fA-F]{4,})>", cm)}
    return {}


def page_texts(data, font):
    """Every text run with the page position the content stream gives it."""
    dec = lambda h: "".join(font.get(h[i:i + 4].lower(), "?")
                            for i in range(0, len(h), 4))
    tok = re.findall(r"(\[.*?\]|<[0-9a-fA-F]*>|[-\d.]+|/\S+|[A-Za-z\*\'\"]+)", data)
    tm = line = [1, 0, 0, 1, 0, 0]
    out = []
    for i, t in enumerate(tok):
        if t == "Tm":
            tm = line = [float(x) for x in tok[i - 6:i]]
        elif t in ("Td", "TD"):
            dx, dy = float(tok[i - 2]), float(tok[i - 1])
            line = [line[0], line[1], line[2], line[3],
                    line[4] + dx * line[0] + dy * line[2],
                    line[5] + dx * line[1] + dy * line[3]]
            tm = line[:]
        elif t == "T*":
            tm = line[:]
        elif t == "TJ":
            s = "".join(dec(h) for h in re.findall(r"<([0-9a-fA-F]+)>", tok[i - 1]))
            if s.strip():
                out.append((s.strip(), tm[4], tm[5]))
    return out


def georeference(texts):
    """Fit page coordinates to British National Grid from the tick labels.

    Northing ticks are printed on both the left and right axes at slightly
    different heights, so only the left column is used - mixing the two
    put a 287 m wobble into an otherwise sub-metre fit.
    """
    E = [(int(m.group(1)), x) for t, x, y in texts
         if (m := re.fullmatch(r"(\d{6})E", t))]
    N = [(int(m.group(1)), x, y) for t, x, y in texts
         if (m := re.fullmatch(r"(\d{6})N", t))]
    xleft = min(n[1] for n in N)
    N = [(v, y) for v, x, y in N if abs(x - xleft) < 1.0]

    def fit(pairs):
        v = np.array([p[0] for p in pairs], float)
        c = np.array([p[1] for p in pairs], float)
        A = np.vstack([c, np.ones_like(c)]).T
        sol, *_ = np.linalg.lstsq(A, v, rcond=None)
        return sol, float(np.max(np.abs(A @ sol - v)))

    # Dedupe: each easting is printed on both the top and bottom axis.
    (ex, e0), eres = fit(sorted(set(E)))
    (ny, n0), nres = fit(sorted(set(N)))
    return dict(e_scale=ex, e_off=e0, n_scale=ny, n_off=n0,
                e_resid=eres, n_resid=nres)


def strips_and_swatches(data):
    """Map-pane image strips and legend swatches, from the `cm ... Do` calls.

    Three kinds of image are placed: the map pane in wide strips, the
    legend swatches, and a locator inset a few hundred units across which
    is neither and is dropped. Splitting on placement width rather than
    pixel width keeps the inset out of the stitched pane.
    """
    pl = re.findall(
        r"q ([\d.]+) 0 0 (-?[\d.]+) ([\d.]+) ([\d.]+) cm\s*/(Im\d+) Do", data)
    big, small = [], []
    for a, b, c, d, n in pl:
        w, h, x, y = float(a), float(b), float(c), float(d)
        if w > 1000:
            big.append((n, w, h, x, y))
        elif w < 60:
            small.append((n, w, h, x, y))
    # Placements are in the flipped space, where y grows DOWNWARDS, so
    # ascending y is top-first on the page. Sorting the other way stacks
    # the map upside down and reads the legend from the bottom up, which
    # silently relabels every preference.
    big.sort(key=lambda s: s[4])           # top strip first
    small.sort(key=lambda s: s[4])         # legend reads downwards
    return big, small


def legend_labels(page):
    """The preference labels, top to bottom, from the page's own text."""
    t = page.extract_text() or ""
    rows = [s.strip() for s in t.split("\n") if s.strip()]
    rows = [s for s in rows if not re.fullmatch(r"\d{6}[EN]", s)]
    if rows and rows[0].lower() != "preference":
        rows = rows[1:]                    # drop the school name
    return [s for s in rows if s.lower() != "preference"]


def extract_page(page, pno):
    data = page.get_contents().get_data().decode("latin-1")
    texts = page_texts(data, page_font(page))
    geo = georeference(texts)
    big, small = strips_and_swatches(data)
    labels = legend_labels(page)
    school = max((t for t, x, y in texts), key=len)

    imgs = {im.name.split(".")[0]: im for im in page.images}
    tiles = [np.asarray(imgs[n].image.convert("RGB")) for n, *_ in big]
    W = max(t.shape[1] for t in tiles)
    canvas = np.vstack([t for t in tiles])
    H = canvas.shape[0]

    # Pane rectangle in page space. Image placements are flipped and scaled.
    x0 = min(s[3] for s in big) * FLIP_SCALE
    x1 = max(s[3] + s[1] for s in big) * FLIP_SCALE
    page_h = float(page.mediabox[3])
    tops = [page_h - FLIP_SCALE * (s[4] + s[2]) for s in big]   # h is negative
    bots = [page_h - FLIP_SCALE * s[4] for s in big]
    ytop, ybot = max(tops + bots), min(tops + bots)

    swatch = {}
    for n, *_ in small:
        s = np.asarray(imgs[n].image.convert("RGBA"))
        op = s[..., 3] > 200
        if op.sum():
            swatch[n] = tuple(int(v) for v in s[..., :3][op].mean(0).round())
    order = [n for n, *_ in small if n in swatch]
    if len(order) != len(labels):
        print(f"  ! page {pno}: {len(order)} swatches vs {len(labels)} labels",
              file=sys.stderr)

    def blobs(rgb, tol):
        d = np.sqrt(((canvas.astype(np.int16) - np.array(rgb)) ** 2).sum(2))
        n, _, st, ct = cv2.connectedComponentsWithStats((d < tol).astype(np.uint8), 8)
        out = []
        for k in range(1, n):
            x, y, w, h, area = st[k]
            if min(w, h) == 0:
                continue
            out.append(dict(area=int(area), ar=max(w, h) / min(w, h),
                            fill=area / (w * h), px=float(ct[k][0]), py=float(ct[k][1])))
        return out

    # Tolerance matters more than anything else here. The catchment overlays
    # are semi-transparent washes, so a symbol colour that happens to sit
    # near one of them starts matching the polygon instead of the dots: the
    # amber symbol holds at 7-8 blobs up to a tolerance of 30 and explodes
    # to 123 blobs and 56,000 px at 60, which is the Varndean catchment
    # fill. Near-black needs to be tighter still, or it picks up road
    # casings and street labels.
    def tol_for(rgb):
        return 18 if max(rgb) < 70 else 30

    cand = {n: blobs(swatch[n], tol_for(swatch[n])) for n in order}
    ref = max(cand.values(), key=lambda v: len([b for b in v if b["area"] >= 40]))
    ar = np.array([b["area"] for b in ref if b["area"] >= 40])
    # The mode of the small blobs, not the median: in a dense catchment more
    # than half the blobs are already clumps, and the median then measures a
    # pair rather than a single symbol and halves every count.
    lone = ar[ar <= np.percentile(ar, 60)]
    unit = float(np.median(lone))
    lo = unit * 0.45

    rows = []
    for n, lab in zip(order, labels):
        for b in cand[n]:
            if b["area"] < lo:
                continue
            k = int(round(b["area"] / unit))
            # A lone symbol is a disc. A clump of them is not, and must not
            # be held to the same shape test - rejecting clumps is what made
            # the first pass undercount the dense schools by half while
            # getting sparse Longhill almost exactly right.
            if k <= 1 and (b["ar"] > 1.7 or b["fill"] < 0.5):
                continue
            if b["ar"] > 6 or b["fill"] < 0.35:
                continue                      # a road, a label, a fill sliver
            e = geo["e_off"] + geo["e_scale"] * (x0 + b["px"] / W * (x1 - x0))
            nn = geo["n_off"] + geo["n_scale"] * (ytop - b["py"] / H * (ytop - ybot))
            rows.append(dict(
                page=pno, school=school, preference=lab,
                easting=round(e, 1), northing=round(nn, 1),
                blob_area_px=b["area"], est_children=max(1, k),
                merged=int(k > 1)))
    return rows, geo, unit, (x1 - x0) * geo["e_scale"] / W, school, labels


def main(src):
    r = PdfReader(src)
    allrows = []
    print(f"{len(r.pages)} pages")
    for i, page in enumerate(r.pages, 1):
        rows, geo, unit, mpx, school, labels = extract_page(page, i)
        n_blob = len(rows)
        n_kids = sum(x["est_children"] for x in rows)
        print(f"  {i}. {school[:38]:38s} {n_blob:4d} blobs -> {n_kids:4d} children"
              f"   fit E{geo['e_resid']:.2f}m N{geo['n_resid']:.2f}m"
              f"   {mpx:.2f} m/px   legend {'/'.join(labels)}")
        allrows += rows

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(allrows[0].keys()))
        w.writeheader()
        w.writerows(allrows)
    print(f"\nwrote {len(allrows)} rows "
          f"({sum(x['est_children'] for x in allrows)} children) to {OUT}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else
         r"C:\Users\adam_\Dropbox\AdamBrighton\SecondarySchools\2025_2026"
         r"\FOI 26370589 Allocations.pdf")
