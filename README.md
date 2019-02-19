# Deedle.Extras

## What is Deedle.Extras
Deedle.Extras is a small library I have written to deal with basic statistical stuff I encounter
in my daily tasks. Most of the heavy lifting is done by Math.NET, and my library simply ties some
of this functionality together with Deedle data frames, so you as the end user has to deal with less mapping and converting
between Deedle and Math.NET datatypes.

## Example 1
The following code is based on having a CSV file that contains the air quality data set from R. The code
does linear regression, where "Ozone" is the dependent variable, and "Solar.R", "Wind", "Temp", "Month", "Day" are the
independent variables. It then produces and prints a summary that looks much like the one i R.

    Frame.ReadCsv("airquality.csv")
    |> Frame.dropSparseRows
    |> Frame.indexRowsInt "Column 1"
    |> LinearRegression.multiDim ["Solar.R"; "Wind"; "Temp"; "Month"; "Day"] "Ozone" (Some "Intercept")
    |> LinearRegression.Fit.summary
    |> printf "%O"

## Example 2
I have also included a few utility functions to easy plotting content from data frames. For example

    Frame.ReadCsv("Foo.csv")
    |> Charting.Frame.Line
Will render an Xplot line chart of all columns, with their column key as label, and row keys on the x-axis.

Hopefully you will find it useful too. Any suggestions and pull requests are more than welcome.
