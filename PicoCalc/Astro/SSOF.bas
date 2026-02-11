'Solar System Object Finder
'V1.0 11.02.2026
'Peter Doerwald
'need V6.02.00

OPTION BASE 1

'GPS Koordinates degrees
Dim lat=50.19835  'negative if south
Dim long=7.85702  'negative if west
Dim zone=1        ' Timezone, here UTC+1
Dim utc$
Dim alt,az
Dim stunde%,minute%,sekunde%
Dim tag%,monat%,jahr%
Dim days%(12)=(31,28,31,30,31,30,31,31,30,31,30,31)
Dim solar$(9)=("SUN","MOON","MERCURY","VENUS","MARS","JUPITER","SATURN","URANUS","NEPTUNE")
Dim x%
CLS

Do
  calcUTC
  Location utc$,lat,long
  Text MM.HRES/2, 6, "** Zachzig presents **", "CM", 7, 1, RGB(BLUE)
  Text MM.HRES/2, 25, "Solar System Object Finder", "CM", 7, 2, RGB(CYAN)
  if MM.INFO$(PLATFORM)="PicoCalc" Then
    Text MM.HRES/2, 50, "UTC:"+utc$+" Bat:"+str$(MM.Info(Battery))+"%  ", "CM", 1, 1, RGB(GREEN)
  else
    Text MM.HRES/2, 50, "UTC:"+utc$+" Bat:", "CM", 1, 1, RGB(GREEN)
  endif  

  line 0,90,319,90,1,RGB(yellow)
  Text 95, 89,"Altitude", "LB", 2, 1, RGB(WHITE)
  Text 210, 89,"Azimuth", "LB", 2, 1, RGB(WHITE)

  for x%=1 to 9
    Text 0, x%*25+88,solar$(x%), "LB", 2, 1, RGB(YELLOW)
    line 0,90+(x%*25),319,90+(x%*25),1,RGB(yellow)
    Execute "Astro "+solar$(x%)+" alt,az"
  
    if alt<0 Then 
      Text 95, x%*25+88,str$(alt,3,4), "LB", 2, 1, RGB(RED)
    else 
      Text 95, x%*25+88,str$(alt,3,4), "LB", 2, 1, RGB(GREEN)
    endif
    Text 210, x%*25+88,str$(az,3,4), "LB", 2, 1, RGB(GREEN)
  next x% 
  pause 1000    
Loop 



sub calcUTC
  Local stNew%
  stunde%=VAL(FIELD$(time$,1,":"))
  minute%=VAL(FIELD$(time$,2,":"))
  sekunde%=VAL(FIELD$(time$,3,":"))
  tag%=VAL(FIELD$(date$,1,"-"))
  monat%=VAL(FIELD$(date$,2,"-"))
  jahr%=VAL(FIELD$(date$,3,"-"))
  
  stNew%=stunde%-zone
  
  'Korrektur negativ
  if (stNew%<0) Then
    stNew%=24+stNew%
    'korrigiere Datum
    tag%=tag%-1
    if(tag%<1) Then
      monat%=monat%-1
      if (monat%<1) Then
        monat%=12
        jahr%=jahr%-1
      endif
      ' Tage im Vormonat setzen (vereinfacht)
      tag%=days%(monat%) 
    endif 
   endif
   'Korrektur overflow
  if (stNew%>23) Then
    stNew%=stNew%-24
    'korrigiere Datum
    tag%=tag%+1
    if (tag%>days%(monat%) Then 
      tag%=1
      monat%=monat%+1
      if monat$>12 then
        monat%=1
        jahr%=jahr%+1
      endif
    endif
   endif        
   utc$=str$(tag%,2,0,"0")+"-"+str$(monat%,2,0,"0")+"-"+str$(jahr%,2,0,"0")
   utc$=utc$+" "+str$(stNew%,2,0,"0")+":"+str$(minute%,2,0,"0")+":"+str$(sekunde%,2,0,"0")    
END SUB  