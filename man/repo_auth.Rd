% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{repo_auth}
\alias{repo_auth}
\title{Authenticated repositories}
\usage{
repo_auth(
  r_version = getRversion(),
  bioc = TRUE,
  cran_mirror = default_cran_mirror(),
  check_credentials = TRUE
)
}
\arguments{
\item{r_version}{R version(s) to use for the Bioconductor repositories,
if \code{bioc} is \code{TRUE}.}

\item{bioc}{Whether to add Bioconductor repositories, even if they
are not configured in the \code{repos} option.}

\item{cran_mirror}{The CRAN mirror to use, see
\code{\link[=default_cran_mirror]{default_cran_mirror()}}.}

\item{check_credentials}{Whether to check that credentials are
available for authenticated repositories.}
}
\value{
Data frame with columns:
\itemize{
\item all columns from the output of \code{\link[=repo_get]{repo_get()}},
\item \code{auth_domains}: authentication domains. pkgcache tries to find the
credentials for these domains, until the search is successful or all
domains fail.
\item \code{auth_domain}: if the credential lookup is successful, then this is
the authentication domain that was used to get the credentials.
\item \code{auth_source}: where the credentials were found. E.g.
\verb{keyring:<backend>} means it was in the default macos keyring.
\item \code{auth_error}: for failed credential searches this is the description
of why the search failed. E.g. maybe the keyring package is not
installed, or pkgcache found no credentials for any of the
authentication domains.
}
}
\description{
pkgcache supports HTTP basic authentication when interacting with
CRAN-like repositories. To use authentication, include a username
in the repo URL:

\if{html}{\out{<div class="sourceCode">}}\preformatted{https://<username>@<repo-host>/<repo-path>
}\if{html}{\out{</div>}}
}
\details{
pkgcache will look up password for this url and username from the
the user's \code{.netrc} file and from the system credential store using
the keyring package. For the URL above it tries the following keyring
keys, in this order:

\if{html}{\out{<div class="sourceCode">}}\preformatted{https://<username>@repo-host/<repo-path>
https://repo-host/<repo-path>
https://<username>@repo-host
https://repo-host
}\if{html}{\out{</div>}}

To add an authenticated repository use \code{\link[=repo_add]{repo_add()}} with the \code{username}
argument. Alternatively, you can set the \code{repos} option directly using
\code{\link[base:options]{base::options()}} and including the username in the repository URL.

\code{repo_auth()} lists authentication information for all configured
repositories.
}
