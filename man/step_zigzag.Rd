% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_zigzag.R
\name{step_zigzag}
\alias{step_zigzag}
\alias{tidy.step_zigzag}
\title{Extract ZigZag features}
\usage{
step_zigzag(recipe, ..., change = 1, percent = TRUE, retrace = FALSE,
  state = FALSE, span = c(0, 0), prefix = "zigzag", h = NULL,
  l = NULL, c = NULL, type = NULL, role = "predictor",
  trained = FALSE, skip = FALSE, id = rand_id("zigzag"))

\method{tidy}{step_zigzag}(x, info = "terms", ...)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{Either three or one (unquoted) column name(s). If three columns
are given, it will represent the \code{"high"}, and \code{"low"} prices, respectively.
Otherwise, if only one column name is given, it will treated
as \code{"close"} price.}

\item{change}{A \code{numeric} vector of length one, to specify the minimum price
change to be considered as the change from peak or trough.}

\item{percent}{A \code{logical} to specify whether the \code{change} argument should
be interpreted as percentage change (\code{TRUE}) or absolute change (\code{FALSE})}

\item{retrace}{A \code{logical} to specify whether the \code{change} argument should
be interpreted as a change from previous move, or from last peak/trough.}

\item{state}{An option to specify whether to return
the current states of the ZigZag. Defaults to \code{FALSE}.}

\item{span}{A \code{numeric} vector of length one to specify the \code{swing} span.
Default to \code{c(0, 0)}; zero addition to backward and forward. See details.}

\item{prefix}{A \code{character} vector of length one that would be used
as a prefix to the created columns.}

\item{h}{A container for the names of \code{"high"}. Leave to \code{NULL}
as it will be populated by \link[recipes:prep.recipe]{prep()} function.}

\item{l}{A container for the names of \code{"low"}. Leave to \code{NULL}
as it will be populated by \link[recipes:prep.recipe]{prep()} function.}

\item{c}{A container for the names of \code{"close"}. Leave to \code{NULL}
as it will be populated by \link[recipes:prep.recipe]{prep()} function.}

\item{type}{A container for the final series type that
would be used (\code{"hl"} or \code{"c"}). Leave to \code{NULL} as it will be
populated by \link[recipes:prep.recipe]{prep()} function.}

\item{role}{For model terms created by this step, what analysis
role should they be assigned? By default, the function assumes
that the created columns will be used
as \code{"predictors"} in a model.}

\item{trained}{A logical to indicate if the necessary informations for
preprocessing have been estimated.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \link[recipes:bake.recipe]{bake()}? While all operations are baked
when \link[recipes:prep.recipe]{prep()} is run, some operations may not
be able to be conducted on new data (e.g. processing
the outcome variable(s)). Care should be taken when using \code{skip = TRUE}
as it may affect the computations for subsequent operations}

\item{id}{A character string that is unique to this step to identify it.}

\item{x}{A \code{step_zigzag} object.}

\item{info}{Options for \code{tidy()} method; whether to return tidied
information for used \code{"terms"} or \code{"params"}}
}
\value{
An updated version of \code{recipe} with the new step
added to the sequence of existing steps (if any).
}
\description{
\code{step_zigzag} creates a \strong{specification} of a recipe
step that will extract \strong{ZigZag} features from an asset price
historical data.
}
\details{
The output from this step are several new columns
which contains the extracted moving average features.

For basic output, this step will produces:
\itemize{
\item \code{value}: the estimated ZigZag value
}

If \code{state} argument is \code{TRUE}, it will also produces:
\itemize{
\item \code{trend}: current trend
\item \code{swing}: swing points; either \code{"up"}, \code{"down"}, or \code{"hold"}
}

Note that the \code{"up"} and \code{"down"} are positioned at the time before
the \code{trend} change; you can control its wide and position using
\code{span} arguments.
}
\examples{

# import libs
library(quantrecipes)

# basic usage
rec <- recipe(. ~ ., data = btcusdt) \%>\%
  step_zigzag(close) \%>\%
  step_naomit(all_predictors()) \%>\%
  prep()

# get preprocessed data
juice(rec)

}
