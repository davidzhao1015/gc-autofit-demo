###########################################################################################
## Functions for creating a JSON file
## Author: Beomsoo Han
## Latest Update: 20150909
############################################################################################

spectrumToJSON.old <- function(d)
{    
  NROW <- nrow(d)        
  ostr <- "{\n \"spectrum_xy\": [\n"
  for(i in 1:(NROW-1)) {
    ostr <- paste(ostr, "\t{ \"x\":", d$x[i], ",\n\t  \"y\":", d$y[i], " },\n", sep="")
  }
  ostr <- paste(ostr, "\t{ \"x\":", d$x[NROW], ",\n\t  \"y\":", d$y[NROW], " } \t ] }", sep="")
  # cat(ostr)
  return(ostr)
}

## generate JSON format string for Spectrum View
spectrumToJSON.fullSpectrum <- function(x, y)
{    
  x.str <- paste("\t\"x\": [", paste(x, collapse=','), "],", sep='')
  y.str <- paste("\t\"y\": [", paste(y, collapse=','), "]", sep='')
  
  res <- paste("\t\"xy_data\": {\n\t", x.str, "\n\t", y.str, "\n\t}")
  
  return(res)
}


spectrumToJSON.fullSpectrum.old <- function(d)
{    
  name.value <- function(i){
    quote <- '';
    # if(class(dtf[, i])!='numeric'){
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ # I modified this line so integers are also not enclosed in quotes
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  objs <- apply(sapply(c("x","y"), name.value), 1, function(x) {paste(x, collapse=', ')})
  objs <- paste('\t\t {', objs, '}')
  
  res <- paste(objs, collapse=',\n')
  res <- paste("\t\"spectrum_xy\": [\n", res, " ]")
  
  return(res)
}

## generate JSON format string for Spectrum View
## Labels
##    x,y,text
##    meta
##       table_data: table columns
##       ms_data: m/z and intensity  
## parameters:
##    table_data: RT, Intensity (Abundence), Text (Comp Name), HMDBID, ....

spectrumToJSON.profile <- function(data.in)
{   
  #  d <- data.frame(x, y, text)
  d <- data.in  
  names(d)[c(4,6,2)] <- c("x","y","text")
  
  # d2 <- data.frame(hmdbid, name)    
  
  name.value <- function(i) {
    quote <- '';
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ 
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  name.value.old <- function(i) {
    quote <- '';
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ 
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  xytext <- apply(sapply(c("x","y","text"), name.value), 1, function(x) { paste(x, collapse=',\t') })
  
  meta <- apply(d, 1, 
                function(x) {                       
                  paste('\t\t"meta": {\n',
                        '\t\t\t\t"table_data": {\n', 
                        '\t\t\t\t\t "HMDB ID": "', x[1], '",\n',
                        '\t\t\t\t\t "Name": "', x[2], '",\n', 
                        '\t\t\t\t\t "RT": "', x[4], '",\n', 
                        '\t\t\t\t\t "RI": "', x[5], '",\n', 
                        '\t\t\t\t\t "Intensity": "', x[6], '",\n', 
                        '\t\t\t\t\t "MatchFactor": "', x[7], '",\n', 
                        '\t\t\t\t\t "TScore": "', x[9], '",\n', 
                        '\t\t\t\t\t "Area": "', x[10], '",\n', 
                        '\t\t\t\t\t "RT(start)": "', x[11], '",\n', 
                        '\t\t\t\t\t "RT(end)": "', x[12], '",\n',
                        '\t\t\t\t\t "Concentration": "', x[15], '"\n',
                        '\t\t\t\t},\n',
                        '\t\t\t\t"ms_data" : {\n',
                        '\t\t\t\t\t "m/z": [', x[13], '],\n', 
                        '\t\t\t\t\t "Intensity": [', x[14], ']\n',
                        '\t\t\t\t}\n',
                        '\t\t\t}\n',
                        sep='') 
                } )
  
  objs <- paste('\t\t{\t', xytext, ",\n\t", meta, '\t\t}', sep='')
  res <- paste(objs, collapse=',\n')
  res <- paste('\t"labels": [\n', res, '\n\t]')
  
  return(res)
}

# xy_data:x,y
# labels:x,y,text; meta:table_data, ms_data

# create_json_file(ofilename, xset.blank@scantime, xset.blank@tic,
# final_PeakProfile_blank$RT, final_PeakProfile_blank$Intensity, final_PeakProfile_blank$Compound,
# mz_data)

create_json_file <- function(ofilename, spectrum.x, spectrum.y, profiled.table)
{
  ## xy_data: x, y      
  fullspec.json <- spectrumToJSON.fullSpectrum(spectrum.x, spectrum.y)
  
  ## Labels: x, y, text
  labels.profiled.json <- spectrumToJSON.profile(profiled.table)
  
  final.json <- paste('{\n', fullspec.json, ',\n', labels.profiled.json, '\n}', sep='')
  cat(final.json, file=ofilename, append=FALSE)
}

create_json_file.old <- function(ofilename, fullspec.x, fullspec.y, profile.x, profile.y, profile.compound)
{
  x <- fullspec.x
  y <- fullspec.y
  spectrum.xy <- data.frame(x,y)
  fullspec.json <- spectrumToJSON.fullSpectrum.old(spectrum.xy)            
  
  x <- profile.x
  y <- profile.y
  peak <- profile.compound
  spectrum.xypeak <- data.frame(x,y,peak)
  profiledspec.json <- spectrumToJSON.profile(spectrum.xypeak)
  
  finalspec.json <- paste("{\n", fullspec.json, ", \n", profiledspec.json, "}")
  cat(finalspec.json, file=ofilename, append=FALSE)    
}

spectrumToJSON.profile.alkane <- function(data.in)
{   
  d <- data.in  
  d$Cn <- paste('C',d[,"Cn"],sep='')
  
  # ALKRT, Intensity, Cn
  names(d)[c(1,3,2)] <- c("x","y","text")
  
  name.value <- function(i) {
    quote <- '';
    if(class(d[, i])!='numeric' && class(d[, i])!= 'integer'){ 
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, d[,i], quote, sep='')
  }
  
  xytext <- apply(sapply(c("x","y","text"), name.value), 1, function(x) { paste(x, collapse=',\t') })

  objs <- paste('\t\t{\t', xytext, '\t}', sep='')
  res <- paste(objs, collapse=',\n')
  res <- paste('\t"labels": [\n', res, '\n\t]')
  
  return(res)
}

create_json_file.alkane <- function(ofilename, spectrum.x, spectrum.y, profiled.table)
{
  ## xy_data: x, y      
  fullspec.json <- spectrumToJSON.fullSpectrum(spectrum.x, spectrum.y)
  
  ## Labels: x, y, text
  labels.profiled.json <- spectrumToJSON.profile.alkane(profiled.table)
  
  final.json <- paste('{\n', fullspec.json, ',\n', labels.profiled.json, '\n}', sep='')
  cat(final.json, file=ofilename, append=FALSE)
}



