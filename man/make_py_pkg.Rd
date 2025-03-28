% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.R
\name{make_py_pkg}
\alias{make_py_pkg}
\title{Make a new python package}
\usage{
make_py_pkg(
  pkg_name,
  path = ".",
  author = "Your Name",
  email = "your.email@example.com",
  description = "A brief description of your library",
  license = "MIT"
)
}
\arguments{
\item{pkg_name}{package name}

\item{path}{project path, default "."}

\item{author}{author}

\item{email}{email}

\item{description}{description}

\item{license}{license}
}
\value{
No return value
}
\description{
Make a new python package
}
\examples{
if (interactive()) {
  make_py_pkg("my_python_package",
    path = "~/projects",
    author = "John Doe", description = "My Python library",
    license = "MIT"
  )
}
}
