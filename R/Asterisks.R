#' @export
Asterisks = function(pvalues)
{
	res=NULL
	for (p.val in pvalues)
	{
		if(is.na(p.val))
		{
			fp = "!"
		} else
		if(p.val>0.1)
		{
			fp = ""
		} else
		if(p.val<=0.1 && p.val>0.05)
		{
			fp = "."
		} else
		if(p.val<=0.05 && p.val>0.01)
		{
			fp = "*"
		} else
		if(p.val<=0.01 && p.val>0.001)
		{
			fp = "**"
		} else
		{
			fp = "***"
		}
		res=c(res,fp)
	}
	return (res)
}