# Add ERCIS cooperate colors.
col.ercis.black    = grDevices::rgb(  0/255,   0/255,   0/255, 1)
col.ercis.grey     = grDevices::rgb( 94/255,  94/255,  93/255, 1)
col.ercis.red      = grDevices::rgb(133/255,  35/255,  57/255, 1)
col.ercis.lightred = grDevices::rgb(200/255, 156/255, 166/255, 1)
col.ercis.blue     = grDevices::rgb(135/255, 151/255, 163/255, 1)
col.ercis.darkblue = grDevices::rgb( 67/255,  92/255, 139/255, 1)
col.ercis.cyan     = grDevices::rgb(  0/255, 156/255, 179/255, 1)
col.ercis.orange   = grDevices::rgb(231/255, 124/255,  18/255, 1)
col.ercis.green    = grDevices::rgb(135/255, 191/255,  42/255, 1)

# Ensure that the output directory is created.
if (!file.exists("output")) {
  dir.create("output")
}

# Set the created "output" directory as a variable for internal use.
dir_presentation <- "output"

# Save the number of CPU Cores in variable.
cpu_cores <- parallel::detectCores()