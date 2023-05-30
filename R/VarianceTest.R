#'@importFrom stats fligner.test bartlett.test
VarianceTest = function(extendedData, y, x, test="Bartlett")
{
	model=as.formula(paste(y,"~",x))
	if(test=="Fligner")
	{
		res=fligner.test(formula=model, data=extendedData)
	}
	if(test=="Bartlett")
	{
		res=bartlett.test(formula=model, data=extendedData)
	}
	return (res)
}