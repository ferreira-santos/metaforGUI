#' metaforGUI: Graphical User Interface (GUI) for the R metafor Package
#' @description
#' Cross-platform GUI for the R metafor[1] package, based on gWidgets and RGtk2. Allows conducting a basic meta-analysis. Suitable for beginners not familiar with R, but does not allow using advanced features of metafor.
#' @seealso
#' \code{\link[metafor]{metafor-package}} for full details of the 'metafor' package.
#' @examples
#' ## Simply run the main function to bring up the GUI.
#' metaforGUI()
#' ## Loading data set, selecting variables and running analysis are run from the GUI.
#' ## Please refer to addtional documentation for details.
#' @references
#' [1] Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import metafor
#' @export
###metaforGUI <- function(){

#Cool Guide: http://user2007.org/program/presentations/verzani-1.pdf
# https://rdrr.io/rforge/gWidgets/

# change icon: http://stackoverflow.com/questions/14764517/how-do-i-change-the-wm-icon-for-a-gwindow-using-gwidgets-in-r

# First run:   install.packages("gWidgetsRGtk2", dep=TRUE)

# You can learn more about package authoring with RStudio at: http://r-pkgs.had.co.nz/
#https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf

#require(gWidgetsRGtk2)
#require(metafor)
options(guiToolkit = "RGtk2")

#Get Screen size (pixels)
sw <- as.numeric(system("wmic desktopmonitor get screenwidth" , intern=TRUE))[-c(1, length(as.numeric(system("wmic desktopmonitor get screenwidth" , intern=TRUE))))][2]
sh <- as.numeric(system("wmic desktopmonitor get screenheight", intern=TRUE))[-c(1, length(as.numeric(system("wmic desktopmonitor get screenheight", intern=TRUE))))][2]
  #print(c("Screen Size:",as.character(sw), as.character(sh)))
start_width <- sw/2
start_height <- sh/2
rm(sh, sw)

win = gwindow("metaforGUI (version-here; FFS)", width=start_width, height=start_height)
rm(start_width, start_height)
#lay = glayout(container=win) # lay = layout organizer
g <- gframe("Define:", horizontal=F, container=win, expand=T)
g_top <- ggroup(container=g)
  glabel("  DataSet file:", container=g_top)
  editFile <- gedit(text="<no file>", container=g_top)
  gbutton("Load file", container=g_top, handler=
    function(h,...) {
    # get locale
    # load file according to locale (read.csv or read.csv2) using --> gfile()
    if(Sys.localeconv()["decimal_point"]==".") { #use read.csv
      read.csv(
        gfile(
          text="Select DataSet file in Comma-Separated Value (CSV) format",
          type="open",
          filter = list(
          "CSV files (*.csv)" = list(patterns = c("*.csv")),
          "All files (*.*)" = list(patterns = c("*")))
          )
        )
    } else { #use read.csv2
      read.csv2(
        gfile(
          text="Select DataSet file in Comma-Separated Value (CSV) format",
          type="open",
          filter = list(
            "CSV files (*.csv)" = list(patterns = c("*.csv")),
            "All files (*.*)" = list(patterns = c("*")))
        )
      )
    }
  })#End-of-btnLoadFile-function

  gseparator(container=g)

g_mid <- ggroup(container=g, expand=T)
  g_mid_left <- ggroup(container=g_mid)
    varbrowser <- gvarbrowser(container=g_mid_left)
  g_mid_right <- ggroup(container=g_mid, horizontal=F)
    addSpring(g_mid_right)

    frameStudies <- gframe("Study names (optional):", container=g_mid_right, expand=T)
      glabel(" ", container=frameStudies)
      btnAddStudies <- gbutton(">>", container=frameStudies, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddStudies)==">>") {
            svalue(labelStudies) <- svalue(varbrowser)
            svalue(btnAddStudies) <- "<<"}
          else {
            svalue(labelStudies) <- "No variable selected"
            svalue(btnAddStudies) <- ">>"}
        })
      labelStudies <- glabel("No variable selected", container=frameStudies)
      glabel("  ", container=frameStudies)
    addSpring(g_mid_right)

    frameES <- gframe("Effect Sizes (ES):", container=g_mid_right, expand=T)
      glabel(" ", container=frameES)
      btnAddES <- gbutton(">>", container=frameES, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddES)==">>") {
            svalue(labelES) <- svalue(varbrowser)
            svalue(btnAddES) <- "<<"}
          else {
            svalue(labelES) <- "No variable selected"
            svalue(btnAddES) <- ">>"}
        })
      labelES <- glabel("No variable selected", container=frameES)
      glabel("  ", container=frameES)
    addSpring(g_mid_right)

    frameVar <- gframe("ES Variances/SE:", container=g_mid_right, expand=T)
      glabel(" ", container=frameVar)
      btnAddVar <- gbutton(">>", container=frameVar, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddVar)==">>") {
            svalue(labelVar) <- svalue(varbrowser)
            svalue(btnAddVar) <- "<<"}
          else {
            svalue(labelVar) <- "No variable selected"
            svalue(btnAddVar) <- ">>"}
        })
      labelVar <- glabel("No variable selected", container=frameVar)
      glabel("  ", container=frameVar)
      radioVar <- gradio(items=c("Variances","Standard Errors (SE)"), container=frameVar)
      glabel("  ", container=frameVar)
    addSpring(g_mid_right)

#    frameN <- gframe("Sample Size (N):", container=g_mid_right, expand=T)
#      glabel(" ", container=frameN)
#      btnAddN <- gbutton(">>", container=frameN, handler=
#        function(h,...) {
#          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
#          else if(svalue(btnAddN)==">>") {
#            svalue(labelN) <- svalue(varbrowser)
#            svalue(btnAddN) <- "<<"}
#          else {
#            svalue(labelN) <- "No variable selected"
#            svalue(btnAddN) <- ">>"}
#        })
#      labelN <- glabel("No variable selected", container=frameN)
#      glabel("  ", container=frameN)
#    addSpring(g_mid_right)

  gseparator(container=g)

g_bottom <- ggroup(container=g)
  glabel("  Output dir (WD):", container=g_bottom)
  editWD <- gedit(getwd(), container=g_bottom)
  gbutton("Change dir", container=g_bottom, handler=
    function(h,...) {
      setwd(gfile(type="selectdir"))
      svalue(editWD) <- getwd()
    })
g2 <- ggroup(horizontal=F, container=win, expand=T)
g2frame <- gframe("Define and Run:", container=g2, horizontal=F, expand=T)
gbutton("Options", container=g2frame) #ISSUE: IDEA: serves as header and clicking brings back defaults!
radioFixedRandom <- gradio(items=c("Fixed Effects","Random Effects"), selected=2, container=g2frame) #ISSUE: consider changing to dropbox with all possible methods
gbutton("Outputs", container=g2frame) #ISSUE: IDEA: serves as header and clicking brings back defaults!
outputMA <- gcheckbox("Meta-Analysis Results", checked=T, container=g2frame, handler=
    function(h,...) {
      if(svalue(outputMA)==F) {gmessage("The Meta-Analysis Results output cannot be disabled as metaforGUI will always produce an output text file (with extension .txt).\n\nThis option is only listed here as a reminder.\n\nNote: the output files will be overwritten everytime you run metaforGUI, so copy or move the files to save them.", "This option cannot be unchecked.")}
      svalue(outputMA) <- T
    })
outputForest <- gcheckbox("Forest Plot (PDF)", checked=T, container=g2frame)
outputFunnel <- gcheckbox("Funnel Plot (PDF)", checked=T, container=g2frame)
outputMAobjData <- gcheckbox("Save Output as RData file", checked=T, container=g2frame)


addSpring(g2frame)
gbutton("Run Meta-Analysis", container=g2frame, handler=
  function(h,...) {
  #ISSUE: IDEA: if files already exist, confirmation dialog asking if user wants to overwrite

  ## Check for btnAddES and btnAddVar if ES and Var variables are defined:
  if(svalue(btnAddES)!="<<" || svalue(btnAddVar)!="<<"){
    #ES and Var variables not assigned
    gmessage("Effect Sizes (ES) or ES Variances/SEs not defined. Please select the variables from the variable list.",title="Required variables not defined.")
  } else {
    #ES and Var variables assigned -> Continue!

  ## Define rma arguments: ##########################
  if(svalue(btnAddStudies)=="<<") { arg_slab <- get(svalue(labelStudies)) }
  arg_yi <- get(svalue(labelES))
  if(svalue(radioVar)=="Variances") {arg_vi<-get(svalue(labelVar));arg_sei<-NULL} else {arg_vi<-NULL;arg_sei<-get(svalue(labelVar))}
  if(svalue(radioFixedRandom)=="Fixed Effects") {arg_method <- "FE"} else {arg_method <- "REML"}

  ## Run rma: ##########################
  if(svalue(btnAddStudies)=="<<") {  #Run with custom Study Names
    meta_analysis <- rma(yi=arg_yi, vi=arg_vi, sei=arg_sei, method=arg_method, slab=arg_slab)
  } else {  #Run without custom Study Names
    meta_analysis <- rma(yi=arg_yi, vi=arg_vi, sei=arg_sei, method=arg_method) }

  ## Save outputs: ##########################
  cat("********** [metaforGUI] Output ***********\n", "Output automatically generated by metaforGUI ",
      as.character(packageVersion("metaforGUI")), " at ", as.character(Sys.time()),".\n\n",
      "List of sections:\n",
      "  1) Function code used\n",
      "  2) Meta-analysis results\n",
      "  3) Meta-analysis publication bias check\n",
      "  4) Additional output files\n",
      "  5) Version and citation details\n\n",
      sep="", file="metaforGUI_output.txt")

  cat("\n********** 1) Function code used ***********\n- metafor Function call:\n    ", file="metaforGUI_output.txt", append=T)
  capture.output(meta_analysis$call, file="metaforGUI_output.txt", append=T)  #Code used to run rma()
  cat("- Effect Sizes (ES):\n    arg_yi =", arg_yi, file="metaforGUI_output.txt", append=T)
  cat("\n- ES Variances*:\n    arg_vi =", arg_vi, file="metaforGUI_output.txt", append=T)
  cat("\n- ES Standard Errors*:\n    arg_sei =", arg_sei, file="metaforGUI_output.txt", append=T)
  cat("\n- Meta-analysis estimation method:\n    arg_method =", arg_method, file="metaforGUI_output.txt", append=T)
  cat("\n\n*(Only one of arg_vi or arg_sei should contain data.\n", file="metaforGUI_output.txt", append=T)

  cat("\n\n********** 2) Meta-analysis results ***********", file="metaforGUI_output.txt", append=T)
  capture.output(summary(meta_analysis), file="metaforGUI_output.txt", append=T)  #Summary of results

  cat("\n********** 3) Meta-analysis publication bias check ***********", file="metaforGUI_output.txt", append=T)
  capture.output(regtest(meta_analysis), file="metaforGUI_output.txt", append=T)  #Regression Test for Funnel Plot Asymmetry

  cat("\n********** 4) Additional output files ***********\n", file="metaforGUI_output.txt", append=T)

  if(svalue(outputForest)==T) {
    pdf("metaforGUI_Forest.pdf"); forest(meta_analysis); dev.off()
    cat("- Forest plot PDF file generated (metaforGUI_Forest.pdf).\n", file="metaforGUI_output.txt", append=T) }

  if(svalue(outputFunnel)==T) {
    pdf("metaforGUI_Funnel.pdf"); funnel(meta_analysis); dev.off()
    cat("- Funnel plot PDF file generated (metaforGUI_Funnel.pdf).\n", file="metaforGUI_output.txt", append=T) }

  if(svalue(outputMAobjData)==T) {
    save("meta_analysis", file="metaforGUI_MetaAnalysis.RData")
    cat("- Meta-analysis results data file generated (metaforGUI_MetaAnalysis.RData). Use load() function to load the data into R.\n", file="metaforGUI_output.txt", append=T) }

  cat("\n\n********** 5) Version and citation details ***********\nWhen using metaforGUI you should cite:\n- ",
      R.version.string, ":\n", sep="", file="metaforGUI_output.txt", append=T)
  capture.output(print(citation(), style="textVersion"), file="metaforGUI_output.txt", append=T)

  cat("\n", paste("- metafor package version", packageVersion("metafor"))," (http://www.metafor-project.org/):\n", sep="", file="metaforGUI_output.txt", append=T)
  capture.output(print(citation("metafor"), style="textVersion"), file="metaforGUI_output.txt", append=T)

  cat("\n", paste("- metaforGUI package version", packageVersion("metaforGUI")), ":\n", sep="", file="metaforGUI_output.txt", append=T)
  capture.output(print(citation("metaforGUI"), style="textVersion"), file="metaforGUI_output.txt", append=T)

  cat("\n\n********** [metaforGUI] End of Output ***********", file="metaforGUI_output.txt", append=T)


  gmessage(paste("Meta-analysis appears to have run successfully!\n\nPlease find the output file(s) in the defined output directory:\n",getwd()), title="Meta-analysis successful")
  cat("\n*metaforGUI* message:\n    Meta-analysis appears to have run successfully!\n    Please find the output file(s) in the defined output directory:\n     ", getwd() ,"\n\n")

  } #End-of-check if ES and Var variables are assigned-If_clause
}) #End-of-btnRunMA_handler_function


###} #End-of-metaforGUI-function / EOF
