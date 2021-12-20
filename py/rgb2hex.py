import matplotlib.pyplot as plt
from matplotlib import colors

def rgb2hex(cname):
  cmap=plt.get_cmap(cname)
  out = []
  for cc in cmap.colors:
      out.append(colors.rgb2hex(cc))
  return out
