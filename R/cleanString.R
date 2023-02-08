#' @title Clean a string.
#' @description Clean a string (removal of special characters and accented characters, removal of multiple spaces).
#' @param x A string.
#' @param toLower If TRUE, the string is transformed to lower case.
#' @return The cleaned string.
#' @keywords internal
#' @importFrom stringr str_to_lower
#' @export
cleanString=function(x,toLower=FALSE)
{
  # Suppression des caracteres speciaux
  x=gsub("[!#$%&'()*+,-./:;<=>?@\n\\^]", " ", x)
  # Suppression des accents
  x=iconv(x, from = 'UTF-8',to="ASCII//TRANSLIT")
  # Suppression des espaces multiples et en debut en fin de chaine
  x=trimws(gsub("\\s+", " ", x))
  # Minuscules
  if (toLower) {
    x=stringr::str_to_lower(x)
  }
  return (x)
}
