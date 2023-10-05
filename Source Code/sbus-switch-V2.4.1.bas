' V2.1.x
' Multikanal Mode für FlySky
' Prog-Karte V2 (5 Bit und Codier-Schalter)

' V2.1.2
' Codeoptimierung: Bit-Picker statt Maskierung
' Compiler 2.0.8.1 (Lizensiert)

' V2.2.0
' Hardware wie 2.1
' PWM-Ausgänge
' 4-fach Check deaktiviert (sollte mit FrSky Radio Firmware 2.1.x nicht mehr nötig sein)
' FlySky-Mode entfernt

' V2.2.1
' Frame-Erkennung überarbeitet. Erlaubt Framing 5ms bis unendlich
'     Timout1: Warten bis 1ms keine Daten gesendet werden, dann aus Daten warten
'     Timout2: Max Wartezeit auf neuen Frame. sonst LED aus, aber kein Fehler
'     Timout3: Max Dauer der Datenübertragung = 4ms, sonst Fehler

' V2.2.2: PWM-Frequenz einstellbar - Prog-Mode erweitert

' V2.3.0: manuelle Kalibrierung

' V2.3.1: PWM-Frequenz ab 15Hz

' V2.4.0:
'     Codeoptimierung bein Einlesen Soft Serial
'     SBUS-Daten in String*22 statt 22 Bytes
'     Bedienung und Progcodes unverändert
'     Korrekturfaktor für Kompatiblität für Multi Module und andere
'     Progcodes gem Excel-Tabelle
'     Test-Mode Endausschläge
'     Code für PWM-Init aus Forum übernommen. Find datasheet page 114

' V2.4.1:
'     PWM-Funktion aus Programmfunktion "Multikanal-Mode" entfernt


$regfile = "ATtiny4313.dat"
$prog &HFF , &HE4 , &HDB , &HFF                             ' intern Oszilator

$hwstack = 32                                               ' default use 32 for the hardware stack
$swstack = 10                                               ' default use 10 for the SW stack
$framesize = 40                                             ' default use 40 for the frame space

$crystal = 8000000


declare sub Magic_Code_R()
declare sub osccal_x()
declare sub Prog1_ok()
declare sub reset_pwm()


Dim A As Byte

Dim Fehler As Bit
Dim Timout As Bit
dim Timtyp as byte

Dim Progdata As Byte
Dim Progdata2 As Byte

Dim Kanal As Byte

Dim schalt_Kanal_ee As Eram Byte         'Kanal in EEProm
Dim schalt_Kanal As Byte

Dim pwm_prescale_ee As Eram Byte                                'PWM-Frequenz in EEProm

Dim pwm_kanal4_ee As Eram Byte
Dim pwm_kanal5_ee As Eram Byte
Dim pwm_Kanal4 As Byte
Dim pwm_Kanal5 As Byte

dim comp_mode as byte   'Kompatibilitäts-Mode aktiv
dim comp_mode_ee as eram byte  'Kompatibilitäts-Mode aktiv

dim osc_x_ee as eram Byte
dim osc_x as byte
dim osc_ram as byte
dim osc_run as byte


Dim Sbstart As Byte                                         'Variablen zum Einlesen SBUS Frame
dim sb_strg as string * 22         ' 16 Kanäle a 11 Bit = 22 Byte = 176 Bit
Dim Sbstop(2) As Byte


Dim Ausgabe As word

Dim Sbus_bit_count As Byte


const blip = 100
Const Timset1 = 224                                         '224 = 1ms
Const Timset2 = 1
Const Timset3 = 120


'Schaltausgänge
Ausg1 Alias Portb.7
Ausg2 Alias Portb.6
Ausg3 Alias Portb.5
Ausg4 Alias Portb.4      'PWM Compare B
Ausg5 Alias Portb.3      'PWM Compare A
Ausg6 Alias Portb.2
Ausg7 Alias Portb.1
Ausg8 Alias Portb.0
Ausg Alias Portb                                            'Schaltausgänge


Progport Alias Pind                                         'Jumper und Programmierkarte
Portd = &B1001_1111                                         'Pull-Ups für Progport setzen
PortA = &B1111_1111                                         'Pull-Ups für PortA

Led1 Alias Portd.5
Config Led1 = Output

Config Ausg = Output                                        'Schaltausgänge als Output (in einem Rutsch wenn alle auf einem Port)
'Config Ausg1 = Output
'Config Ausg2 = Output
'Config Ausg3 = Output
'Config Ausg4 = Output
'Config Ausg5 = Output
'Config Ausg6 = Output
'Config Ausg7 = Output
'Config Ausg8 = Output


' Timer für Frameerkennung
' Prescale 256 = 0,032ms
' Startwert 224 (32 bis Overflow-Interrupt) = 1 ms
Config Timer0 = Timer , Prescale = 256
On Timer0 Timerinterr



Led1 = 1                                                    'kurz zeigen dass Saft drauf ist
waitms blip
Led1 = 0


'EE-Prom in Variablen schreiben

schalt_kanal = schalt_kanal_ee
pwm_kanal4 = pwm_kanal4_ee
pwm_kanal5 = pwm_kanal5_ee
comp_mode = comp_mode_ee



Progstart:

Progdata = Not Progport          'Prog-Karte gesteckt ?
Progdata = Progdata And &B0001_1111

select case schalt_kanal
   case 1 to 18
   case else
      progdata = 31              'lade Defaults wenn Werte undefiniert
end select



select case Progdata

   case 0 to 15
      goto Progend    'nix machen / Jumper nicht gesteckt


   case 16
      call Prog1_ok()
      schalt_kanal = progdata2  + 33
      goto Progend


   case 17     'Multikanal Mode
      call reset_pwm()
      call Prog1_ok()
      select case Progdata2
         case 1
            call reset_pwm()
            schalt_kanal_ee = 17
         case 2
            call reset_pwm()
            schalt_kanal_ee = 18
         case else
            goto Prog_nok
      end select


   case 18     'Einzelkanal Mode
      call Prog1_ok()
         schalt_kanal = progdata2  + 1
         schalt_kanal_ee = schalt_kanal
         call reset_pwm()


   case 19     'PWM Ausg 4
      call Prog1_ok()
         pwm_kanal4 = progdata2 + 1
         pwm_kanal4_ee = pwm_kanal4


   case 20     'PWM Ausg 5
      call Prog1_ok()
         pwm_kanal5 = progdata2 + 1
         pwm_kanal5_ee = pwm_kanal5


   case 21     'PWM Fequenz
      call Prog1_ok()
      select case Progdata2
         case 10 to 14
            pwm_prescale_ee = progdata2
         case else
            goto Prog_nok
         end select


   case 22     'Kompatibilitäts-Mode
      call Prog1_ok()
      select case Progdata2
         case 0 to 1
            comp_mode_ee = progdata2
         case else
            goto Prog_nok
         end select


   case 30                  'OSCCAL kalibrieren
      osc_run = 1
      osc_ram = osccal
      goto Progend


   case 31                         'Reset to default
      call reset_pwm()
      comp_mode_ee = 0             ' 0
      osc_x_ee = 0                 ' 0
      schalt_kanal_ee = 17         ' 17


   case else
      goto prog_nok     'ungültiger wert

end select




Prog2_ok:
   Led1 = 1
   waitms blip
   Led1 = 0
   waitms blip
   Led1 = 1
   waitms blip
   led1 = 0
   Wait 1
   Goto Prog2_ok

Prog_nok:
   Led1 = Not Led1
   Waitms 66
   Goto Prog_nok


Progend:

osc_x = osc_x_ee
select case osc_x
   case 1 to 15       ' OSCCAL-Korrektur setzen wenn Wert gesetzt
      osc_x = osccal + osc_x
      osccal = osc_x - 8
end select


'Prescale @ 8MHz:
'1 = 15625Hz
'8 =  1953Hz
'64 = 244Hz
'256 = 61Hz
'1024 = 15Hz


Const P1 = &B000_001
Const P8 = &B000_010
Const P64 = &B000_011
Const P256 = &B000_100
Const P1024 = &B000_101

Tccr1a = &B0000_0001                                        'WGM10 for PWM, Phase Correct, 8-bit
If Pwm_kanal4 > 0 Then Set Tccr1a.com1b1
If Pwm_kanal5 > 0 Then Set Tccr1a.com1A1

 Select Case Pwm_prescale_ee
   Case 10
      Tccr1b = P1024
'   case 11 'B
'      Tccr1b = P256
   Case 12
      Tccr1b = P64
   case 13 'D
      Tccr1b = P8
   Case 14
      Tccr1b = P1
   case else
      Tccr1b = P256
 End Select




Open "COMD.6:100000,8,E,2, inverted" For Input As #1      'Softserial initialisieren

Enable Timer0         'Timer für Frame-Erkennung




Do



Inputstart:

if osc_run = 1 then call osccal_x      'Kalibrierungsprozess aktiv


Timer0 = Timset1                                       'Timer setzen
timtyp = 1
Enable Interrupts
Timout = 0


Wartpause:
   While Timout = 0
      If Pind.6 = 1 Then Timer0 = Timset1                 'Pin wie SoftUART - Setzt Interrupt-Timer zurüch solange Daten empfangen werden
   Wend


Fehler = 0

Timer0 = timset2
timtyp = 2                                    'Timer setzen für  max Wartezeit bis Datenübertragung - nur LED1 wird ausgeschaltet - Ausgänge werden geschaltet
Enable Interrupts


'einlesen des SBUS Frame
      Inputbin #1 , Sbstart
Timer0 = Timset3
timtyp = 3                                    'Timer setzen für  max Dauer der Datenübertragung, sonst Fehler

      Inputbin #1 , Sb_strg     ' 16 Kanäle a 11 Bit = 22 Byte = 176 Bit

 '     Inputbin #1 , Sbflag                                  'Flag-Byte
      Inputbin #1 , Sbstop(1) ; 2                                  'muss ein Stopbyte sein sonst ist was schief gelaufen

disable interrupts

'Fehlercheck

      If Sbstart = &B00001111 Then
      Else
         Fehler = 1
      End If

      Sbstop(1) = Sbstop(1) And &B11110000                        'Failsafe Flag ignorieren um Failsafe des Empfängers nutzen zu können

      If Sbstop(1) = 0 Then
      Else
         Fehler = 1
      End If

      If Sbstop(2) = 0 Then
      Else
         Fehler = 1
      End If

      If Fehler = 1 Then
         Led1 = 0
         Goto Inputstart                                    'wenn Fehler dann nochmal einlesen
      Else
         Led1 = 1
      End If


'wenn keine Fehler, dann Daten auswerten und Ausgänge setzen

Select Case schalt_kanal                                  'Aktion je nach Mode - siehe auch Plausibilität in Progmode

   Case 1 To 16      'Enzelkanal Mode

      kanal = schalt_kanal
      call magic_code_r()

   Case 17   'Multikanal 1-8
      Ausgabe.0 = sb_strg.10
      Ausgabe.1 = sb_strg.21
      Ausgabe.2 = sb_strg.32
      Ausgabe.3 = sb_strg.43
      Ausgabe.4 = sb_strg.54
      Ausgabe.5 = sb_strg.65
      Ausgabe.6 = sb_strg.76
      Ausgabe.7 = sb_strg.87

   Case 18   'Multikanal 9-16
      Ausgabe.0 = sb_strg.98
      Ausgabe.1 = sb_strg.109
      Ausgabe.2 = sb_strg.120
      Ausgabe.3 = sb_strg.131
      Ausgabe.4 = sb_strg.142
      Ausgabe.5 = sb_strg.153
      Ausgabe.6 = sb_strg.164
      Ausgabe.7 = sb_strg.175

   Case 33 to 49    'Check Endausschlag
      kanal = schalt_kanal -32
      call magic_code_r()
      if ausgabe = 0 or ausgabe = 255 then
         led1 =1
      else
         led1 = 0
      end if
      goto Inputstart

End Select


' Schaltausgänge setzen
Ausg = Flip(ausgabe)        ' kompletter Port gedreht (bei Tiny4313)



'PWM Ausgänge setzen

if pwm_kanal4 > 0 then
   kanal = pwm_kanal4
   call magic_code_r()
   Compare1b = ausgabe      'PWM-Ausgang setzen
end if

if pwm_kanal5 > 0 then
   kanal = pwm_kanal5
   call magic_code_r()
   Compare1a = ausgabe     'PWM-Ausgang setzen
end if




Loop



Timerinterr:                                                'Timer-Interrupt setzt Variable "Timout"

Disable Interrupts

select case timtyp
   case 1  'warte auf Frame-Pause - kein Fehler
      Timout = 1
   case 2  'Fehler Frame-Pause > 8ms
      led1 = 0
   case 3  'Fehler Frame > 4ms
      led1 = 0
      fehler = 1

end select


Return




sub magic_code_r()

'----------------------  Magic Code  -------------------------------------------
'Eingabe-Variable: "kanal" - Kanal (1-16) der ausgewertet wird
'Ausgabe-Variable: "ausgabe" - Kanalwert 8bit


Sbus_bit_count = kanal * 11
Sbus_bit_count = Sbus_bit_count - 11
ausgabe = 0

for a = 0 to 10
ausgabe.a = sb_strg.Sbus_bit_count
incr Sbus_bit_count
next


'Korrekturfaktor für Kompatibilitäts-Mode
if comp_mode = 1 then
'Max-Min begrenzen
if Ausgabe < 205 then ausgabe = 205
if Ausgabe > 1837 then ausgabe = 1837


'Faktor
   ausgabe = ausgabe * 10
   ausgabe = ausgabe / 64

'3 bit nach unten schieben
'   ausgabe = ausgabe / 8

'in die Mitte rücken
   ausgabe = ausgabe - 32

else
   ausgabe = ausgabe / 8


end if

end sub


sub reset_pwm()

pwm_kanal4_ee = 0            ' 0
pwm_kanal5_ee = 0            ' 0
pwm_prescale_ee = 11         ' 11

end sub



sub Prog1_ok()

blink1:
   Led1 = 1
   waitms blip
   led1 = 0
   Wait 1
   Progdata2 = Not Progport      'Prog-Karte erneut einlesen
   Progdata2 = Progdata2 And &B0001_1111
if progdata2.4 = 1 then Goto blink1  ' solange blinken bis Jumper gezogen


End Sub



sub osccal_x()

Progdata = Not Progport      'Prog-Karte erneut einlesen

osc_x = progdata And &B0000_1111

if progdata.4 = 0 then
   osc_x_ee = osc_x
   goto Prog2_ok
end if


if osc_x = 0 then
   osccal = osc_ram
else
   osc_x = osc_ram + osc_x      'übertaktet ist unproblematischer als untertaktet für Soft-Serial
   osccal = osc_x - 8
end if


end sub