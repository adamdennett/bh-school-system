# Policy simulator

A Shiny app for experimenting with the Brighton & Hove secondary school
system: change what families want, how many places each school offers,
where one school sits, which catchment map is in force and what year it
is, and watch every objective move at once.

```r
shiny::runApp("app")
```

## What it runs

The spatial interaction model from section 7 of the strategic view, at
its fullest rung — weighted preferences, a capacity ceiling, a catchment
term and competing destinations. It is **not** a lookup over precomputed
scenarios: the questions the app exists to answer are continuous ones
("how attractive would Longhill have to be to fill 210 places?") and no
precomputed grid answers those. 165 neighbourhoods by 10 schools runs in
milliseconds, so it runs on every slider move.

`app/R/check.R` asserts that it still reproduces the published M4
figures school by school. Run it after any change to the model:

```
Rscript app/R/check.R
```

It currently agrees to 0.00 children across all ten schools.

## The pieces

| File | What it is |
|---|---|
| `app.R` | UI and server |
| `R/model.R` | the spatial interaction model and the capacity ceiling |
| `R/outcomes.R` | scoring a run on places, money, fairness and travel |
| `R/check.R` | agreement with the published model |
| `data/sim_inputs.rds` | built by `R/05_app_inputs.R` in the repository root |

Rebuild the inputs after any change to `data/`:

```
Rscript R/05_app_inputs.R
```

## The point

The objectives pull against each other, so the app never shows one
without the others. Make Longhill as wanted as Dorothy Stringer and it
fills — and two other schools fall below their admission numbers, and
segregation gets slightly worse. Cut admission numbers to fix the
finances and journeys lengthen. The strip along the top is there so that
no tab can be optimised out of sight of the rest.

The "What this is not" tab carries the limits, and they matter: the
model is uncalibrated, attractiveness is a single number per school with
no theory of how it would be changed, children cannot leave the city,
and the two faith schools are the least reliable rows in it.
