option_list = list(
  optparse::make_option(c("-f", "--filepath"), action="store", default=Sys.getenv("COVID_OUTPUT_PATH","model_output"), type='character', help="path to the output directory")
)
opts = optparse::parse_args(optparse::OptionParser(option_list=option_list))
all_files <- list.files(opts$filepath,recursive=TRUE,full.names=TRUE) ## List relative to model_output
for(file in all_files){
  new_file <- gsub(':','_',file)
  new_file <- gsub('[.]*/','/',new_file)
  new_dir <- dirname(new_file)
  print(new_dir)
  dir.create(new_dir,recursive=TRUE)
  file.rename(file,new_file)
}