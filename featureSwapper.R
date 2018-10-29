# WHAT     : Script for swapping feature IDs  in a dataframe. 
#            Mapping dataframe required for swapping. 
#            A mapping dataframe should contain a minimal of 
#            two columns, one named 'current', the other `previous`,
#            One row corresponds to one matching pair.
#            Each row j of mapping df, one feature pair. 
#            Values are checked cell by cell (df[k,i]).,
#            in df with values to be swapped.
#            If df[k,i] found in $current[j], swap for $previous[j]


# INPUT    : 4 required user defined arguments,
#           `mapping_df_filepath`       : absolut path to .csv filemapping dataframe
#           `to_be_swapped_df_filepath` : absolut path to .csv file with dataframe that will be swapped 
#           `output_file_ID`            : the output csv with the swapped df will inherit the filename of
#                                         the dataframe that served as template (eg. current cohorts XCMS ids)
#                                         and an extra identifier (output_file_ID) will also be added.
#           `output_dir_path`           : the absolut path to savedir


# RETURNS      : A list with 3 objects:
#                list[1]: the initial unswapped df
#                list[2]: the swapped df 
#                list[3]: the swapped df's filepath
# Extract like so:
#
# (before_swap_df <- as.data.frame(swapped_df_N60[1]))
# (swapped_df     <- as.data.frame(swapped_df_N60[2])) 
# (FILE           <- as.character (swapped_df_N60[3]))

# HOW TO CALL  : eg in rmarkdown block

# ```{r}
# source(featureSwapper_filepath)
# featureSwapper(mapping_df_filepath, to_be_swapped_df_filepath, output_file_ID, output_dir_path)
# ```

featureSwapper <- function(mapping_df_filepath, to_be_swapped_df_filepath, output_file_ID, output_dir_path){
  library(dplyr)
  # Load dataframes
  mapping_df         = read.csv(mapping_df_filepath      , sep = ",", check.names = FALSE, stringsAsFactors = FALSE)
  pre_swap_df      = read.csv(to_be_swapped_df_filepath, sep = ",", check.names = FALSE, stringsAsFactors = FALSE)

  # Convert to character; get rid of factors with levels
  # levels prevent assignment of df[k,l] to new value and generate error
  
  mapping_df[,]          = lapply(mapping_df[,], as.character)
  
  # Select only columns with features (pattern to grep "ref" using dplyr::select)
  to_be_swapped_df       = dplyr::select(pre_swap_df, contains("ref"))
  to_be_swapped_df[,]    = lapply(to_be_swapped_df[,], as.character)
  
  #########################################################
  # S T A R T  of looping over dataframe values one by one:
  #########################################################
  
  # COLUMNS: Loop over all columns that contains `ref` in colnamebut the first
  for (i in 1:length(colnames(to_be_swapped_df))){
    
    # ROWS: Loop over every row
    for (k in 1:length(rownames(to_be_swapped_df))){
      
      # Store metabolite of current being checked:
      current_XCMS_feat_examined = to_be_swapped_df[k,i]
      
      # numeric if feature found, NA if not found;
      index_of_found_metabolite = match(current_XCMS_feat_examined, mapping_df$current) 
      
      
      # Check if in mapping_df$current contains current_XCMS_feat_examined
      if      ( is.na(index_of_found_metabolite)){
        to_be_swapped_df[k,i] <- "NA"
      }
      
      else if (!is.na(index_of_found_metabolite)){
        to_be_swapped_df[k,i] <- mapping_df$previous[index_of_found_metabolite]
        
      }
    }
  } 
  
  swapped_df      <- to_be_swapped_df
  swapped_df$flag <- pre_swap_df$flag
  
  
  
  # Set path of (i)output directory (ii) filename:
  savedir    = output_dir_path 
  FILE       = paste0(savedir, output_file_ID,  basename(to_be_swapped_df_filepath))
  
  # Write dataframe into .csv file
  write.table( swapped_df,  
               file      = FILE,
               append    = FALSE, 
               quote     = FALSE, 
               sep       = ",",
               row.names = F,
               col.names = T)
  
 return(list(pre_swap_df, swapped_df, FILE)) 
};

