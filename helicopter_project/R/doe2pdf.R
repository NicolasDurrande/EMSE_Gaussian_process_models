library(tikzDevice)

single_helico_PS <- function(X,expNumber,groupName){
  # function inputs
  Ww = X[1]
  Wl = X[2]
  Tl = X[3]
  Al = 25+sqrt( (Tl-25)^2 + Wl^2 - 2*(Tl-25)*Wl*cos(X[4]/180*pi+pi/2))
  # parameters
  Aw = 7
  Tb = 10
  Tw = 12
  lineHeight = 5
  # create string
  helico_str = sprintf("\\begin{tikzpicture}[x=1mm,y=1mm]
  \\draw (%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f);
  \\draw (%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f)--(%f,%f);
  \\draw (%f,%f)--(%f,%f);
  \\draw [dashed] (%f,%f)--(%f,%f);
  \\draw [dashed] (%f,%f)--(%f,%f);
  \\draw [dashed] (%f,%f)--(%f,%f);
  \\node at (0,%f) {\\footnotesize{%s}};
  \\node at (0,%f) {\\footnotesize{exp %i}};
  \\node at (0,%f) {\\footnotesize{$W_w = %.2f$ }};
  \\node at (0,%f) {\\footnotesize{$W_l = %.2f$}};
  \\node at (0,%f) {\\footnotesize{$T_l = %.2f$}};
  \\node at (0,%f){\\footnotesize{$\\theta = %.2f$  }};
\\end{tikzpicture}",
		0,Wl+Al, -Aw,Wl+Al, -Aw,Wl, -Ww,Wl, -Ww,0, -Tw,-Tb, -Tw,-Tl, 0,-Tl,
		0,Wl+Al,  Aw,Wl+Al,  Aw,Wl,  Ww,Wl,  Ww,0,  Tw,-Tb,  Tw,-Tl, 0,-Tl,
		0,Wl+Al,0,0,
		-Aw,Wl,Aw,Wl,
		-Ww,0,Ww,0,
		-Aw,Wl+Al-25,Aw,Wl+Al-25,
		-Tb-1,groupName,
		-Tb-1-lineHeight,expNumber,
		-Tb-1-3*lineHeight,Ww,
		-Tb-1-2*lineHeight,Wl,
		-Tb-1-4*lineHeight,Tl,
		-Tb-1-5*lineHeight,X[4])
	return(helico_str)
}

drawHelicopters <- function(X,groupName){
  filename <- paste0("helicos_",groupName,".tex")

  ## header
  cat(file=filename, "\\documentclass[a4paper]{article}
\\usepackage[top=5mm,bottom=5mm,right=5mm,left=5mm]{geometry}
\\usepackage{tikz}

\\begin{document}

")  
  
  ## helicopters
  wleft = 200
  for(i in 1:nrow(X)){
    if(2*X[i,1] > wleft){
      cat(file=filename,append=TRUE,"\n \n")
      wleft = 200
    }
    cat(file=filename,append=TRUE,single_helico_PS(X[i,],i,groupName))
    wleft <- wleft-2*X[i,1]
  }
  ## tail
  cat(file=filename,append=TRUE,"\n \n\\end{document}")
  
  tools::texi2pdf(filename,clean=TRUE)
  tmp <- file.remove(filename)
}

###########
# # example
# Ww_bounds = c(20, 50)
# Wl_bounds = c(30, 75)
# Tl_bounds = c(50, 80)
# alpha_bounds = c(-15, 25)
# 
# # DoE
# X = expand.grid(Ww_bounds,Wl_bounds,Tl_bounds,alpha_bounds)
# 
# # Make pdf
# drawHelicopters(X,"HeureuxCopter")
  
