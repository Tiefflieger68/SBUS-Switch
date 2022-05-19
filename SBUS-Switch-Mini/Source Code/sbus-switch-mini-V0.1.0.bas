
'Basis SBUS-Svitch-V2.4.1
'Anpassungen an Tiny 45/85 Hardware
'Prog Mode angepasst
'Codeoptimierung f�r Schaltausg�nge
'Ausg3
'Pinbelegung nach Schaltplan
'Anpassungen OCR1A/B f�r T85



'f�r Tiny45:
'$regfile="attiny45.dat"


'f�r Tiny85:
$regfile="attiny85.dat"


$PROG &HFF,&HE2,&HDD,&HFF' generated.

$crystal=8000000

$hwstack = 32                                               ' default use 32 for the hardware stack
$swstack = 10                                               ' default use 10 for the SW stack
$framesize = 40



declare sub Magic_Code()

CONST Updateeprom=1   'eeProm nur bei �nderung schreiben
dim vers As Eram Byte
vers = 0

Dim A As Byte

Dim Fehler As Bit
Dim Timout As Bit
dim Timtyp as byte
dim progmode as byte

Dim Kanal As Byte
Dim Kanal1_ee As ERAM Byte
Dim Kanal1 As Byte
Dim Kanal2_ee As ERAM Byte
Dim Kanal2 As Byte
'Dim Kanal3_ee As ERAM Byte     'kanal3 = kanal2 + 1
Dim Kanal3 As Byte

dim pwm_mode_ee as ERAM byte
dim pwm_mode as byte

Dim Sbstart As Byte                                         'Variablen zum Einlesen SBUS Frame
dim sb_strg as string * 22         ' 16 Kan�le a 11 Bit = 22 Byte = 176 Bit
Dim Sbstop(2) As Byte


Dim Ausgabe As word

Dim Sbus_bit_count As Byte

const blip = 100
Const Timset1 = 224                                         '224 = 1ms
Const Timset2 = 1
Const Timset3 = 120

ein alias 0
aus alias 1


'Schaltausg�nge
Ausg1 Alias Portb.1     'PWM Compare A
Ausg2 Alias Portb.4     'PWM Compare B
Ausg3 Alias Portb.2
sbusin alias pinb.0                                 'Schaltausg�nge

Led1 Alias Portb.3
Progport Alias Pinb.3                                         'Jumper und Programmierkarte



'Config Led1 = Output        'erst nach Prog-Abfrage auf Output setzen

Config Ausg1 = Output
Config Ausg2 = Output
Config Ausg3 = Output


Progport = 1            'Pullup f�r LED/Progport



' Timer f�r Frameerkennung
' Prescale 256 = 0,032ms
' Startwert 224 (32 bis Overflow-Interrupt) = 1 ms
Config Timer0 = Timer , Prescale = 256
On Timer0 Timerinterr


'################################################################################################

'     In-Code-Settings:
'     alle Parameter sind zu aktivieren, um Programmieren �ber Jumper/Sender zu deaktivieren
'     Alle Einstellwerte sind hier zu setzen


'kanal1 = 14        '1-16
'kanal2 = 15        '1-16
'kanal3 = 16        '1-16
'pwm_mode = 3       '0-3

'goto in_code_set


'################################################################################################




Progstart:

waitms 10       ' warten bis Progport "High"
if Progport = 1 then goto Progend        'Prog-Jumper gesteckt ?

progmode = 1

bitwait progport , set       'wenn Jumper gezogen gehts weiter

Progend:



'EE-Prom pr�fen und in RAM schreiben

select case kanal1_ee
   case 1 to 16
      Kanal1 = Kanal1_ee
   case else
      kanal1 = 1
end select


select case kanal2_ee
   case 1 to 16
      Kanal2 = Kanal2_ee
   case else
      kanal2 = 2
end select


if kanal2 <= 15 then kanal3 = kanal2 + 1 else kanal3 = 16
pwm_mode = pwm_mode_ee


in_code_set:      'Abfrage Prog-Jumper und EE-Prom to RAM �berspringen wenn In-Code_Settings aktiv


select case pwm_mode
   case 1
      Config Timer1 = Pwm , Compare A Pwm = Clear Up , Prescale = 256
   case 2
      Config Timer1 = Pwm , Compare B Pwm = Clear Up , Prescale = 256
   case 3
      Config Timer1 = Pwm , Compare A Pwm = Clear Up , Compare B Pwm = Clear Up , Prescale = 256
end select
'Prescale 64 = 488Hz@8MHz
'Prescale 128 = 244Hz@8MHz
'Prescale 256 = 122Hz@8MHz
'Prescale 512 = 61Hz@8MHz


Config led1 = output
Led1 = ein                                                    'kurz zeigen dass Saft drauf ist
waitms blip
Led1 = aus


Open "COMB.0:100000,8,E,2, inverted" For Input As #1      'Softserial initialisieren

Enable Timer0         'Timer f�r Frame-Erkennung



Do
Inputstart:

Timer0 = Timset1                                       'Timer setzen
timtyp = 1
Enable Interrupts
Timout = 0

Wartpause:
   While Timout = 0
      If sbusin = 1 Then Timer0 = Timset1                 'Pin wie SoftUART - Setzt Interrupt-Timer zur�ch solange Daten empfangen werden
   Wend

Fehler = 0

Timer0 = timset2
timtyp = 2                                    'Timer setzen f�r  max Wartezeit bis Daten�bertragung - nur LED1 wird ausgeschaltet - Ausg�nge werden geschaltet
Enable Interrupts


'einlesen des SBUS Frame
      Inputbin #1 , Sbstart
Timer0 = Timset3
timtyp = 3                                    'Timer setzen f�r  max Dauer der Daten�bertragung, sonst Fehler

      Inputbin #1 , Sb_strg     ' 16 Kan�le a 11 Bit = 22 Byte = 176 Bit

      Inputbin #1 , Sbstop(1) ; 2                                  'muss ein Stopbyte sein sonst ist was schief gelaufen

disable interrupts

'Fehlercheck

      If Sbstart = &B00001111 Then
      Else
         Fehler = 1
      End If

      Sbstop(1) = Sbstop(1) And &B11110000                        'Failsafe Flag ignorieren um Failsafe des Empf�ngers nutzen zu k�nnen

      If Sbstop(1) = 0 Then
      Else
         Fehler = 1
      End If

      If Sbstop(2) = 0 Then
      Else
         Fehler = 1
      End If

      If Fehler = 1 Then
         Led1 = aus
         Goto Inputstart                                    'wenn Fehler dann nochmal einlesen
      Else
         Led1 = ein
      End If


'wenn keine Fehler, dann Daten auswerten und Ausg�nge setzen


if progmode = 0 then goto no_prog        'Programmierung aus SBUS einlesen und EE schreiben

kanal1 = 0
kanal2 = 0
pwm_mode = 0

For kanal = 16 To 1 Step -1
   call magic_code

   select case ausgabe
   case 0 to 31               'Ausg2 = Schaltausg
      pwm_mode.1 = 0
      kanal2 = kanal
   case 32 to 95              'Ausg2 = PWM-Dimm
      pwm_mode.1 = 1
      kanal2 = kanal
   'case 96 to 159             'neutral

   case 160 to 223            'Ausg1 = PWM-Dimm
      pwm_mode.0 = 1
      kanal1 = kanal
   case 223 to 255            'Ausg1 = Schaltausg
      pwm_mode.0 = 0
      kanal1 = kanal
   end select

next


'RAM to EE-Prom
kanal1_ee = kanal1
kanal2_ee = kanal2
pwm_mode_ee = pwm_mode

Prog_ok:
   Led1 = ein
   waitms blip
   Led1 = aus
   waitms blip
   Led1 = ein
   waitms blip
   led1 = aus
   Wait 1
Goto Prog_ok


no_prog:

'Schaltausg�nge setzen
Sbus_bit_count = kanal1 * 11
Sbus_bit_count = Sbus_bit_count - 1
ausg1 = sb_strg.Sbus_bit_count

Sbus_bit_count = kanal2 * 11
Sbus_bit_count = Sbus_bit_count - 1
ausg2 = sb_strg.Sbus_bit_count

Sbus_bit_count = kanal3 * 11
Sbus_bit_count = Sbus_bit_count - 1
ausg3 = sb_strg.Sbus_bit_count


'PWM Ausg�nge setzen
kanal = kanal1
call magic_code()
'Compare1A = ausgabe      'PWM-Ausgang setzen - Alias OCR1A/B nutzen wg Bug in Bascom
OCR1A = ausgabe      'PWM-Ausgang setzen

kanal = kanal2
call magic_code()
OCR1B = ausgabe     'PWM-Ausgang setzen



Loop


Timerinterr:                                                'Timer-Interrupt setzt Variable "Timout"

Disable Interrupts

select case timtyp
   case 1  'warte auf Frame-Pause - kein Fehler
      Timout = 1
   case 2  'Fehler Frame-Pause > 8ms
      led1 = aus
   case 3  'Fehler Frame > 4ms
      led1 = aus
      fehler = 1
end select

Return





sub magic_code()

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


'Korrekturfaktor f�r Kompatibilit�ts-Mode (bei T45/T85 immer aktiv)

'Max-Min begrenzen
if Ausgabe < 205 then ausgabe = 205
if Ausgabe > 1837 then ausgabe = 1837

'Faktor und 3 bit nach unten schieben
   ausgabe = ausgabe * 10
   ausgabe = ausgabe / 64

'in die Mitte r�cken
   ausgabe = ausgabe - 32

end sub


end