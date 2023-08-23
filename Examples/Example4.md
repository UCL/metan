<a href ="https://www.mrcctu.ucl.ac.uk/"><img src="../logo_ukri-mrc-ctu_transparent-background.png" width="50%" /></a>

# Example 4
Confidence and predictive intervals represented by overlaid shading

Simulated example dataset by Ross Harris, 2006

<a href ="https://github.com/UCL/metan/blob/main/Examples"><img src="Example4_Harris.png" width="75%" alt="forest plot" /></a>

```Stata
use http://fmwww.bc.edu/repec/bocode/m/metan_example_data, clear
metan tdeath tnodeath cdeath cnodeath, rd random rfdist lcols(id) counts ///
	forestplot(ocilineopts(color(gs8) hide) rfcilineopts(color(gs12)) graphregion(color(white)) classic ///
	xlabel(, format("%03.1f")) favours(Favours treatment # Favours control))
```

