'Temperaturmessung PicoCalc
'V1.0 30.09.2025
'Peter Doerwald
'
'Temperature with DS18B20 at PIN GP2

pi=3.14159 '265359
x=MM.HRES/2
y=MM.VRES/2
r=MM.VRES/2-1
Dim h!

FRAMEBUFFER LAYER  'L
FRAMEBUFFER CREATE 'F

FRAMEBUFFER WRITE L
h!=0
Do
  CLS
  Scale
  Bat
  TEMPR START GP2
  Pause 500
  h!=TEMPR(GP2)
  If (h!<>1000) Then Hand h!
  FRAMEBUFFER MERGE
  Pause 10
Loop


Sub Hand hb!
 Static w,e
 e=hb!
 e=(e-15)/10
 w=(180-(e*30))*Pi/180
 x2=Sin(w)*85+x
 y2=Cos(w)*85+y
 Line x,y,x2,y2,3,RGB(blue)
 Circle x,y,15,3,,RGB(blue),RGB(yellow)
 'Print digital temp
 Color RGB(black),RGB(yellow)
 Font #7
 If (hb!<0) Then
  Print @(x-17,y+57) hb!
 Else
  Print @(x-19,y+57) hb!
 EndIf
 Color RGB(black),RGB(white)
 Print @(x-6,y+72) Chr$(96);"C"
End Sub

Sub Scale
CLS RGB(white)
Static w,e,f,g

For f=-30 To 60 Step 10
 e=(f-15)/10
 w=(180-(e*30)-k)*Pi/180

 xa=Sin(w)*88+x
 ya=Cos(w)*88+y
 xb=Sin(w)*100+x
 yb=Cos(w)*100+y
 xc=Sin(w)*125+x
 yc=Cos(w)*125+y
 Color RGB(black),RGB(white)
 Font #2
 Print @(xc-20,yc-8) f
 Line xa,ya,xb,yb,2,RGB(black)
 If (f<60) Then
   For g=1 To 9
    e=(f+g-15)/10
    w=(180-(e*30)-k)*Pi/180
    xa=Sin(w)*90+x
    ya=Cos(w)*90+y
    xb=Sin(w)*94+x
    yb=Cos(w)*94+y
 xc=Sin(w)*100+x
    yc=Cos(w)*100+y
 If (g=5) Then
     Line xa,ya,xc,yc,1,RGB(black)
 Else
   Line xa,ya,xb,yb,1,RGB(black)
 EndIf
   Next
 EndIf

Next
Circle x,y,150,5,,RGB(cyan)
'Paint temperature ranges
Arc x,y,102,110,-135,10,RGB(blue)
Arc x,y,102,110,11,30,RGB(green)
Arc x,y,102,110,31,137,RGB(red)
Box x-20,y+50,40,20,,RGB(brown),RGB(yellow)
End Sub

Sub Bat
Static Ba!
Ba!=MM.Info(Battery)*2/10
Box x-11,y+100,22,12,,RGB(black),RGB(white)
Box x+11,y+103,3,6,,RGB(black),RGB(white)
Box x-10,y+101,Ba!,10,,RGB(green),RGB(green)
End Sub
