# install_dependencies.R

required_packages <- c("testthat", "dplyr", "httr")  # Add only the packages you need

# Install packages if not already installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
