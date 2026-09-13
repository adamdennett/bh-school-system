/* app/www/map.js — the map, in plain Leaflet.
   ------------------------------------------------------------------
   This used to be the R leaflet package. That package imports sf, and
   sf brings terra, sp, raster, s2 and units with it, so a map of ten
   dots was costing the deployment the entire compiled geospatial
   stack. The projection now happens once when app/data/sim_inputs.rds
   is built, and the server sends this file lon/lat numbers and GeoJSON
   text, which is all Leaflet ever wanted.

   Esri's attribution stays visible: that is what the free tier is in
   exchange for. */

(function () {
  var map = null, shapes = null, dots = null, legend = null;

  function ensure() {
    if (map) return map;
    var el = document.getElementById("map");
    if (!el || typeof L === "undefined") return null;

    map = L.map(el, { preferCanvas: true }).setView([50.845, -0.14], 12);

    var esri = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/";
    var opts = { maxNativeZoom: 16, maxZoom: 20 };
    L.tileLayer(esri + "World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
      Object.assign({
        attribution: 'Tiles &copy; <a href="https://www.esri.com/">Esri</a> ' +
          '&mdash; Esri, HERE, Garmin, &copy; ' +
          '<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>' +
          ' contributors'
      }, opts)).addTo(map);
    L.tileLayer(esri + "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
      Object.assign({}, opts)).addTo(map);

    shapes = L.layerGroup().addTo(map);
    dots = L.layerGroup().addTo(map);
    return map;
  }

  /* The card the map sits in is laid out after the map is made, so the
     tiles can be cut off until Leaflet is told the size changed. */
  function nudge() { if (map) setTimeout(function () { map.invalidateSize(); }, 0); }

  /* The colour legend. Built once, from the stops the server sends with
     the dots, so the scale on the map and the scale in the legend are the
     same numbers. Bottom left, clear of the attribution. */
  function drawLegend(lg) {
    if (!lg || legend) return;
    var lo = lg.stops[0].at, hi = lg.stops[lg.stops.length - 1].at;
    var pos = function (v) { return (v - lo) / (hi - lo) * 100; };
    legend = L.control({ position: "bottomleft" });
    legend.onAdd = function () {
      var div = L.DomUtil.create("div", "map-legend");
      var grad = lg.stops.map(function (s) {
        return s.col + " " + pos(s.at).toFixed(1) + "%";
      }).join(", ");
      var ticks = lg.ticks.map(function (t) {
        var p = pos(t.at);
        return '<span style="position:absolute;top:0;left:' + p.toFixed(1) +
          '%;transform:translateX(-' + p.toFixed(0) + '%);white-space:nowrap">' +
          t.lab + '</span>';
      }).join("");
      div.innerHTML =
        '<div class="ml-title">' + lg.title + '</div>' +
        '<div class="ml-sides"><span>' + lg.left + '</span><span>' + lg.right + '</span></div>' +
        '<div class="ml-bar" style="background:linear-gradient(to right,' + grad + ')"></div>' +
        '<div class="ml-ticks">' + ticks + '</div>';
      return div;
    };
    legend.addTo(map);
  }

  Shiny.addCustomMessageHandler("map_draw", function (msg) {
    if (!ensure()) return;
    shapes.clearLayers();
    dots.clearLayers();

    if (msg.geojson) {
      L.geoJSON(JSON.parse(msg.geojson), {
        style: { fillColor: "#8aa0b4", fillOpacity: 0.10,
                 color: "#5a6b7c", weight: 1.2 },
        onEachFeature: function (f, layer) {
          if (f.properties && f.properties.grp)
            layer.bindTooltip(String(f.properties.grp), { sticky: true });
        }
      }).addTo(shapes);
    }

    (msg.dots || []).forEach(function (d) {
      L.circleMarker([d.lat, d.lon], {
        radius: d.r, color: "#333333", weight: 1,
        fillColor: d.col, fillOpacity: 0.85
      }).bindTooltip(d.lab, { sticky: true }).addTo(dots);
    });

    drawLegend(msg.legend);
    nudge();
  });

  window.addEventListener("resize", nudge);
})();
