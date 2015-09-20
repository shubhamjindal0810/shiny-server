import sys
sys.path.insert(0,'/home/demo/shiny-server/')

import code128
def jindsh(data,name):
  code=code128.code128_format(data)
  img=code128.code128_image(code)
  img.save(name)
