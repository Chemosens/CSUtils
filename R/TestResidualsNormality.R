#' @importFrom stats ks.test shapiro.test
TestResidualsNormality = function(residual, test="KolmogorovSmirnov")
{
	if(test=="ShapiroWilks")
	{
		res=shapiro.test(residual)
	}
    if(test=="KolmogorovSmirnov")
	{
		res=ks.test(residual,"pnorm")
	}
	return (res)
}