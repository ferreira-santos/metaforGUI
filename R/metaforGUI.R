#' metaforGUI: Graphical User Interface (GUI) for the R metafor Package
#' @description
#' Cross-platform GUI for the R metafor[1] package, based on gWidgets and RGtk2. Allows conducting a basic meta-analysis. Suitable for beginners not familiar with R, but does not allow using advanced features of metafor.
#' @seealso
#' \code{\link[metafor]{metafor-package}} for full details of the 'metafor' package.
#' @examples
#' ## Simply run the main function to bring up the GUI.
#' metaforGUI()
#' ## Loading data set, selecting variables and running analysis are run from the GUI.
#' ## Please refer to addtional documentation for details: https://github.com/ferreira-santos/metaforGUI
#' @references
#' [1] Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import RGtk2
#' @import cairoDevice
#' @import metafor
#' @import grDevices
#' @import utils
#' @export

metaforGUI <- function(csv.dec = Sys.localeconv()["decimal_point"], csv.sep = ","){       # For toolkit option, replace "){" --> gui.tk = "RGtk2"){
  #CITATION: citation("metaforGUI")

  if(csv.dec==",") { csv.sep <- ";" }

  #Option to change the guiToolkit (under development)
  # if (gui.tk=="RGtk2" | gui.tk=="tcltk") { options(guiToolkit = gui.tk)
  # } else { options(guiToolkit = "RGtk2") } #Define default here, and in the function call
  options(guiToolkit = "RGtk2")

  #Get Screen size (pixels)
  sw <- as.numeric(system("wmic desktopmonitor get screenwidth", intern=TRUE)[-c(1, length(system("wmic desktopmonitor get screenwidth", intern=TRUE)))])
  sh <- as.numeric(system("wmic desktopmonitor get screenheight", intern=TRUE)[-c(1, length(system("wmic desktopmonitor get screenheight", intern=TRUE)))])
  #print(c("Screen Size:",as.character(sw), as.character(sh)))
  start_width <- sw/2
  start_height <- sh/2
  rm(sh, sw)

  win_main = gwindow(paste0("metaforGUI (version ",packageDescription("metaforGUI")$Version,"; FFS)"), width=start_width, height=start_height)
  rm(start_width, start_height)
  # lay = glayout(container=win) # lay = layout organizer
  win <- ggroup(horizontal=T, container=win_main, expand=T)

  ## GUI: LEFT FRAME ####
  g <- gframe("Define:", horizontal=F, container=win, expand=T)

  ## LF Top: Load Dataset ####
  g_top <- ggroup(container=g)
  glabel("  DataSet file:", container=g_top)
  editFile <- gedit(text="<no file>", container=g_top)
  gbutton("Load file", container=g_top, handler=
            function(h,...) {
              # load Dataset file
              tag(win, "f") <- choose.files(caption="Select DataSet file in Comma-Separated Value (CSV) format", multi=F,
                                            filters=matrix(c("CSV files (*.csv)","All files (*.*)","*.csv","*.*"), ncol=2), index=1)
              if(length(tag(win, "f"))>0) {
                svalue(editFile) <- tag(win, "f")
                # read according to locale (read.csv or read.csv2)
                tryCatch(
                  tag(win,"dataset") <<- read.csv(tag(win, "f"), dec=csv.dec, sep=csv.sep),
                  error = function(e) {
                    gmessage(icon="error", paste0("An error occurred while trying to load the CSV dataset file. CSV files may differ depending on regional/locale settings: if the decimal mark is a dot ( . ) then CSV files use commas ( , ) to separate values, but if the decimal mark is a comma ( , ) then CSV files use semicolons ( ; ) to separate values.\n\nmetaforGUI attempted to determine these values automatically and considered:\ndecimal mark = ",csv.dec,"\nseparator = ",csv.sep,"\n\nYou may open your CSV file in a text editor to check what is the actual decimal mark and separator being used in that file. Then you may define these values manually by restarting metaforGUI defining the function arguments csv.dec and csv.sep, for example by typing:\nmetaforGUI(csv.dec=\",\", csv.sep=\";\")\n\nPlease see package documentation for further details.\n\nFor reference and bug reporting, the R error message was the following:\n\"",e$message,"\""), title="Error loading file")
                    cat(paste0("\n*metaforGUI* message:\n    An error occurred while trying to load the CSV dataset file. CSV files may differ depending on regional/locale settings: if the decimal mark is a dot ( . ) then CSV files use commas ( , ) to separate values, but if the decimal mark is a comma ( , ) then CSV files use semicolons ( ; ) to separate values.\n\n    metaforGUI attempted to determine these values automatically and considered:\n    decimal mark = ",csv.dec,"\n    separator = ",csv.sep,"\n\n    You may open your CSV file in a text editor to check what is the actual decimal mark and separator being used in that file. Then you may define these values manually by restarting metaforGUI defining the function arguments csv.dec and csv.sep, for example by typing:\n      metaforGUI(csv.dec=\",\", csv.sep=\";\")\n\n    Please see package documentation for further details.\n\n    For reference and bug reporting, the R error message was the following:\n    \"",e$message,"\"\n\n"))
                  }
                )
                try(assign("dataset", tag(win,"dataset"), envir=.GlobalEnv), silent=T)
              }
            })#End-of-btnLoadFile-function

  gseparator(container=g)

  ## LF Mid: VarBrowser & Var Selection ####
  g_mid <- ggroup(container=g, expand=T)
  g_mid_left <- ggroup(container=g_mid)

  varbrowser <- gvarbrowser(container=g_mid_left)

  g_mid_right <- ggroup(container=g_mid, horizontal=F)
  addSpring(g_mid_right)

  frameStudies <- gframe("Study names (optional):", container=g_mid_right, expand=T)
  glabel(" ", container=frameStudies)
  btnAddStudies <- gbutton(">>", container=frameStudies, handler=
                             function(h,...) {
                               if(svalue(varbrowser)=="") { gmessage("<No variable selected> from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
                               else if(svalue(btnAddStudies)==">>") {
                                 svalue(labelStudies) <- svalue(varbrowser)
                                 svalue(btnAddStudies) <- "<<"}
                               else {
                                 svalue(labelStudies) <- "<No variable selected>"
                                 svalue(btnAddStudies) <- ">>"}
                             })
  labelStudies <- glabel("<No variable selected>", container=frameStudies)
  glabel("  ", container=frameStudies)
  addSpring(g_mid_right)

  frameES <- gframe("Effect Sizes (ES):", container=g_mid_right, expand=T)
  glabel(" ", container=frameES)
  btnAddES <- gbutton(">>", container=frameES, handler=
                        function(h,...) {
                          if(svalue(varbrowser)=="") { gmessage("<No variable selected> from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
                          else if(svalue(btnAddES)==">>") {
                            svalue(labelES) <- svalue(varbrowser)
                            svalue(btnAddES) <- "<<"}
                          else {
                            svalue(labelES) <- "<No variable selected>"
                            svalue(btnAddES) <- ">>"}
                        })
  labelES <- glabel("<No variable selected>", container=frameES)
  glabel("  ", container=frameES)
  addSpring(g_mid_right)

  frameVar <- gframe("ES Variances/SE:", container=g_mid_right, expand=T)
  glabel(" ", container=frameVar)
  btnAddVar <- gbutton(">>", container=frameVar, handler=
                         function(h,...) {
                           if(svalue(varbrowser)=="") { gmessage("<No variable selected> from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
                           else if(svalue(btnAddVar)==">>") {
                             svalue(labelVar) <- svalue(varbrowser)
                             svalue(btnAddVar) <- "<<"}
                           else {
                             svalue(labelVar) <- "<No variable selected>"
                             svalue(btnAddVar) <- ">>"}
                         })
  labelVar <- glabel("<No variable selected>", container=frameVar)
  glabel("  ", container=frameVar)
  radioVar <- gradio(items=c("Variances","Standard Errors (SE)"), container=frameVar)
  glabel("  ", container=frameVar)
  addSpring(g_mid_right)

  #    frameN <- gframe("Sample Size (N):", container=g_mid_right, expand=T)
  #      glabel(" ", container=frameN)
  #      btnAddN <- gbutton(">>", container=frameN, handler=
  #        function(h,...) {
  #          if(svalue(varbrowser)=="") { gmessage("<No variable selected> from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
  #          else if(svalue(btnAddN)==">>") {
  #            svalue(labelN) <- svalue(varbrowser)
  #            svalue(btnAddN) <- "<<"}
  #          else {
  #            svalue(labelN) <- "<No variable selected>"
  #            svalue(btnAddN) <- ">>"}
  #        })
  #      labelN <- glabel("<No variable selected>", container=frameN)
  #      glabel("  ", container=frameN)
  #    addSpring(g_mid_right)

  gseparator(container=g)

  ## LF Bottom: Output Folder ####
  g_bottom <- ggroup(container=g)
  glabel("  Output Folder/Directory (WD):", container=g_bottom)
  editWD <- gedit(getwd(), container=g_bottom)
  gbutton("Change Folder", container=g_bottom, handler=
            function(h,...) {
              d <- choose.dir(getwd(), caption = "Select Output Folder")
              if(!is.na(d)) {
                setwd(d)
                svalue(editWD) <- getwd()
              }
            })

  ## GUI: RIGHT FRAME ####
  g2 <- ggroup(horizontal=F, container=win, expand=T)
  g2frame <- gframe("Define and Run:", container=g2, horizontal=F, expand=T)
  glabel("Options", container=g2frame) #ISSUE: IDEA: serves as header and clicking brings back defaults!
  radioFixedRandom <- gradio(items=c("Fixed Effects","Random Effects"), selected=2, container=g2frame) #ISSUE: consider changing to dropbox with all possible methods
  glabel("Outputs", container=g2frame) #ISSUE: IDEA: serves as header and clicking brings back defaults!
  outputMA <- gcheckbox("Meta-Analysis Results", checked=T, container=g2frame, handler=
                          function(h,...) {
                            if(svalue(outputMA)==F) {
                              gmessage("The Meta-Analysis Results output cannot be disabled as metaforGUI will always produce an output text file (with extension .txt).\n\nThis option is only listed here as a reminder.\n\nNote: the output files will be overwritten everytime you run metaforGUI, so copy or move the files to save them.", title="This option cannot be unchecked.")
                              cat("\n*metaforGUI* message:\n    The Meta-Analysis Results output cannot be disabled as metaforGUI will always produce an output text file (with extension .txt).\n    This option is only listed here as a reminder.\n    Note: the output files will be overwritten everytime you run metaforGUI, so copy or move the files to save them.\n\n")
                            }
                            svalue(outputMA) <- T
                          })
  outputForest <- gcheckbox("Forest Plot (PDF)", checked=T, container=g2frame)
  outputFunnel <- gcheckbox("Funnel Plot (PDF)", checked=T, container=g2frame)
  outputMAobjData <- gcheckbox("Save Output as RData file", checked=T, container=g2frame)

  gbutton("Run Meta-Analysis", container=g2frame, handler=
            function(h,...) {
              #ISSUE: IDEA: if files already exist, confirmation dialog asking if user wants to overwrite

              ## Check for btnAddES and btnAddVar if ES and Var variables are defined:
              if(svalue(btnAddES)!="<<" || svalue(btnAddVar)!="<<"){
                #ES and Var variables not assigned
                gmessage(icon="error", "Effect Sizes (ES) or ES Variances/SEs not defined. Please select the variables from the variable list.",title="Error: Required variables not defined.")
              } else {
                #ES and Var variables assigned -> Continue!

                ## Define rma arguments ##########################
                # arg_slab = Study Labels (optional)
                if(svalue(btnAddStudies)=="<<") {
                  if (length(strsplit(as.character(svalue(labelStudies)), "\\$")[[1]])==1)
                  { tag(win,"arg_slab") <- get(svalue(labelStudies)) }
                  else
                  { tag(win,"arg_slab") <- with(get(strsplit(as.character(svalue(labelStudies)), "\\$")[[1]][1]), get(strsplit(as.character(svalue(labelStudies)), "\\$")[[1]][2])) }
                }

                # arg_yi = Effect Sizes (ES)
                if (length(strsplit(as.character(svalue(labelES)), "\\$")[[1]])==1) {
                  tag(win, "arg_yi") <- get(svalue(labelES))
                }
                else {
                  tag(win,"arg_yi") <- with(get(strsplit(as.character(svalue(labelES)), "\\$")[[1]][1]), get(strsplit(as.character(svalue(labelES)), "\\$")[[1]][2]))
                }

                # arg_vi OR arg_sei = ES Variances OR ES Standard Errors
                if (length(strsplit(as.character(svalue(labelVar)), "\\$")[[1]])==1) {
                  if(svalue(radioVar)=="Variances") {
                    tag(win,"arg_vi")<-get(svalue(labelVar)); tag(win,"arg_sei")<-NULL }
                  else {
                    tag(win,"arg_vi")<-NULL; tag(win,"arg_sei")<-get(svalue(labelVar)) }
                }
                else {
                  if(svalue(radioVar)=="Variances") {
                    tag(win,"arg_vi")<-with(get(strsplit(as.character(svalue(labelVar)), "\\$")[[1]][1]), get(strsplit(as.character(svalue(labelVar)), "\\$")[[1]][2])); tag(win,"arg_sei")<-NULL
                  }
                  else {
                    tag(win,"arg_vi")<-NULL; tag(win,"arg_sei")<-with(get(strsplit(as.character(svalue(labelVar)), "\\$")[[1]][1]), get(strsplit(as.character(svalue(labelVar)), "\\$")[[1]][2]))
                  }
                }

                # arg_method = Meta-analytic method
                if(svalue(radioFixedRandom)=="Fixed Effects") { tag(win,"arg_method") <- "FE"} else {tag(win,"arg_method") <- "REML" }

                ## Run rma ##########################
                if(length(tag(win,"arg_yi"))<3 || (length(tag(win,"arg_yi"))!=length(tag(win,"arg_vi")) && length(tag(win,"arg_yi"))!=length(tag(win,"arg_sei")))) {
                  gmessage(icon="error", paste("One of the following problems was detected with the data entered.\n\n1) Meta-analysis cannot be computed with less than 3 effect sizes/studies (the current dataset only contains", length(tag(win,"arg_yi")),"data points).\n\n2) The number of effect sizes, variances/SEs, and/or study labels do not match. Please ensure that all variables have the same number of data points.\n\nOutputs will not be produced."), title="Error: Problems found with the data.")
                  cat(paste("\n*metaforGUI* message:\n    One of the following problems was detected with the data entered.\n      1) Meta-analysis cannot be computed with less than 3 effect sizes/studies (the current dataset only contains", length(tag(win,"arg_yi")),"data points).\n      2) The number of effect sizes, variances/SEs, and/or study labels do not match. Please ensure that all variables have the same number of data points.\n    Outputs will not be produced.\n\n"))
                } else if(svalue(btnAddStudies)=="<<" && (length(tag(win,"arg_yi"))!=length(tag(win,"arg_slab")))) {
                  gmessage(icon="error", paste("One of the following problems was detected with the data entered.\n\n1) Meta-analysis cannot be computed with less than 3 effect sizes/studies (the current dataset only contains", length(tag(win,"arg_yi")),"data points).\n\n2) The number of effect sizes, variances/SEs, and/or study labels do not match. Please ensure that all variables have the same number of data points.\n\nOutputs will not be produced."), title="Error: Problems found with the data.")
                  cat(paste("\n*metaforGUI* message:\n    One of the following problems was detected with the data entered.\n      1) Meta-analysis cannot be computed with less than 3 effect sizes/studies (the current dataset only contains", length(tag(win,"arg_yi")),"data points).\n      2) The number of effect sizes, variances/SEs, and/or study labels do not match. Please ensure that all variables have the same number of data points.\n    Outputs will not be produced.\n\n"))
                } else {

                  if(svalue(btnAddStudies)=="<<") {
                    #Run WITH custom Study Names
                    tag(win,"meta_analysis") <- rma(yi=tag(win,"arg_yi"), vi=tag(win,"arg_vi"), sei=tag(win,"arg_sei"), method=tag(win,"arg_method"), slab=tag(win,"arg_slab"))
                  } else {
                    #Run WITHOUT custom Study Names
                    tag(win,"meta_analysis") <- rma(yi=tag(win,"arg_yi"), vi=tag(win,"arg_vi"), sei=tag(win,"arg_sei"), method=tag(win,"arg_method")) }

                  ## Save outputs ##########################
                  cat("********** [metaforGUI] Output ***********\n", "Output automatically generated by metaforGUI ",
                      as.character(packageVersion("metaforGUI")), " on ", as.character(Sys.time()),".\n\n",
                      "List of sections:\n",
                      "  1) Function code used\n",
                      "  2) Meta-analysis results\n",
                      "  3) Meta-analysis publication bias check (Egger's test)\n",
                      "  4) Additional output files\n",
                      "  5) Version and citation details\n\n",
                      sep="", file="metaforGUI_Output.txt")

                  cat("\n********** 1) Function code used ***********\n- metafor Function call:\n    ", file="metaforGUI_Output.txt", append=T)
                  #capture.output(tag(win,"meta_analysis")$call, file="metaforGUI_Output.txt", append=T)  # Actual code used to run rma() -- but gets complex because of tag() function
                  cat("    rma(yi = arg_yi, vi = arg_vi, sei = arg_sei, method = arg_method)", file="metaforGUI_Output.txt", append=T)
                  cat("\n- Effect Sizes (ES):\n    arg_yi =", tag(win,"arg_yi"), file="metaforGUI_Output.txt", append=T)
                  cat("\n- ES Variances*:\n    arg_vi =", tag(win,"arg_vi"), file="metaforGUI_Output.txt", append=T)
                  cat("\n- ES Standard Errors*:\n    arg_sei =", tag(win,"arg_sei"), file="metaforGUI_Output.txt", append=T)
                  cat("\n- Meta-analysis estimation method:\n    arg_method =", tag(win,"arg_method"), file="metaforGUI_Output.txt", append=T)
                  cat("\n\n*(Only one of arg_vi or arg_sei should contain data).\n", file="metaforGUI_Output.txt", append=T)

                  cat("\n\n********** 2) Meta-analysis results ***********", file="metaforGUI_Output.txt", append=T)
                  capture.output(summary(tag(win,"meta_analysis")), file="metaforGUI_Output.txt", append=T)  #Summary of results

                  cat("\n********** 3) Meta-analysis publication bias check ***********", file="metaforGUI_Output.txt", append=T)
                  capture.output(regtest(tag(win,"meta_analysis")), file="metaforGUI_Output.txt", append=T)  #Egger's Regression Test for Funnel Plot Asymmetry

                  cat("\n********** 4) Additional output files ***********\n", file="metaforGUI_Output.txt", append=T)
                  if(svalue(outputForest)==T) {
                    pdf("metaforGUI_Forest.pdf"); forest(tag(win,"meta_analysis")); dev.off()
                    cat("- Forest plot PDF file generated (metaforGUI_Forest.pdf).\n", file="metaforGUI_Output.txt", append=T) }

                  if(svalue(outputFunnel)==T) {
                    pdf("metaforGUI_Funnel.pdf"); funnel(tag(win,"meta_analysis")); dev.off()
                    cat("- Funnel plot PDF file generated (metaforGUI_Funnel.pdf).\n", file="metaforGUI_Output.txt", append=T) }

                  if(svalue(outputMAobjData)==T) {
                    meta_analysis <- tag(win,"meta_analysis")
                    save(meta_analysis, file="metaforGUI_MetaAnalysis.RData")
                    cat("- Meta-analysis results data file generated (metaforGUI_MetaAnalysis.RData). Use load() function to load the data into R.\n", file="metaforGUI_Output.txt", append=T) }

                  cat("\n\n********** 5) Version and citation details ***********\nWhen using metaforGUI you should cite:\n\n- ",
                      R.version.string, ":\n", sep="", file="metaforGUI_Output.txt", append=T)
                  capture.output(print(citation(), style="textVersion"), file="metaforGUI_Output.txt", append=T)

                  cat("\n", paste("- metafor package version", packageVersion("metafor"))," (http://www.metafor-project.org/):\n", sep="", file="metaforGUI_Output.txt", append=T)
                  capture.output(print(citation("metafor"), style="textVersion"), file="metaforGUI_Output.txt", append=T)

                  cat("\n", paste("- metaforGUI package version", packageVersion("metaforGUI")), ":\n", sep="", file="metaforGUI_Output.txt", append=T)
                  capture.output(print(citation("metaforGUI"), style="textVersion"), file="metaforGUI_Output.txt", append=T)

                  cat("\n\n********** [metaforGUI] End of Output ***********", file="metaforGUI_Output.txt", append=T)


                  gmessage(paste("Meta-analysis appears to have run successfully!\n\nPlease find the output file(s) in the defined output directory:\n",getwd()), title="Meta-analysis successful")
                  cat("\n*metaforGUI* message:\n    Meta-analysis appears to have run successfully!\n    Please find the output file(s) in the defined output directory:\n     ", getwd() ,"\n\n")

                } #End-of-check if there are at least 3 data-points
              } #End-of-check if ES and Var variables are assigned-If_clause
            }) #End-of-btnRunMA_handler_function
  addSpring(g2frame)

  aboutText <- gtext("Copyright 2017 Fernando Ferreira-Santos\nFor details and documentation please visit the website ",
                     height=300, container=g2frame)
  insert(aboutText,"https://github.com/ferreira-santos/metaforGUI", font.attr=c(style="italic"), do.newline=F)
  enabled(aboutText) <- F
  aboutButton <- gbutton("Visit website",container=g2frame, handler=
                           function(h,...) {browseURL("https://github.com/ferreira-santos/metaforGUI#metaforgui")})


} #End-of-metaforGUI-function / EOF
