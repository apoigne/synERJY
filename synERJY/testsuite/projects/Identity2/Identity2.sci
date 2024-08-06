function [x,y,typ]=Identity2(job,arg1,arg2)
x=[];y=[];typ=[];
select job
case 'plot' then
  standard_draw(arg1)
case 'getinputs' then
  [x,y,typ]=standard_inputs(arg1)
case 'getoutputs' then
  [x,y,typ]=standard_outputs(arg1)
case 'getorigin' then
  [x,y]=standard_origin(arg1)
case 'define' then
  rpar=[]
  ipar=[]
  model=scicos_model()
  model.sim=list('se_Identity2',4)
  model.in=[2]
  model.out=[2]
  model.evtin=[]
  model.evtout=[]
  model.rpar=rpar'
  model.ipar=ipar'
  model.blocktype='c'
  model.firing=[]
  model.dep_ut=[%t,%f]
  model.nzcross=0
  exprs=[
        ]
  gr_i=['xstringb(orig(1),orig(2),''Identity2'',sz(1),sz(2),''fill'');']
  x=standard_define([3 2],model,exprs,gr_i)
end
endfunction
