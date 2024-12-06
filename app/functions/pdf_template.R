template_pdf <- function(file){
  # Create temporary directory
  temp_dir <- tempdir()
  logos_dir <- file.path(temp_dir, "logos")
  dir.create(logos_dir, showWarnings = FALSE)
  
  # Write the LaTeX header file to temp_dir
  header_path <- file.path(temp_dir, "header.tex")
  cat('\\usepackage{fancyhdr}
            \\usepackage[margin=1in]{geometry}
            \\usepackage{graphicx}
            \\usepackage{color}
            
            \\fancypagestyle{watermark}{
              \\fancyfootoffset{15pt}
              \\renewcommand{\\headrulewidth}{0pt}
              \\fancyhf{}
              \\cfoot{\\textcolor{gray!30}{\\scalebox{4}{TarSpot Forecast}}}
            }
            \\pagestyle{watermark}
            \\AtBeginDocument{\\thispagestyle{watermark}}', 
      file = header_path)
  
  # Specify the path to your report template in the working directory
  original_template <- "report_template.Rmd"  # Update with the correct location of your Rmd file
  
  # Copy the report template to the temporary directory
  if (!file.exists(original_template)) {
    showNotification("Original report template not found in working directory.", type = "error")
    stop("Original report template not found in working directory.")
  }
  
  # Copy the report template to temp_dir
  report_template <- file.path(temp_dir, "report_template.Rmd")
  file.copy(original_template, report_template, overwrite = TRUE)
  
  # List of images
  images <- c("OPENSOURDA_color-flush.png", "PLANPATHCO_color-flush.png", "DATASCIE_color-flush.png")
  
  # Copy images to logos_dir within temp_dir
  for (img in images) {
    img_path <- file.path(getwd(), "logos", img)
    if (!file.exists(img_path)) {
      showNotification(paste("Image not found:", img), type = "error")
      stop(paste("Image not found:", img))
    }
    file.copy(img_path, file.path(logos_dir, img), overwrite = TRUE)
  }
  return(report_template)
}