'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_OSID_v.1.0
'  date: 06/05/2007
'  prot: 1.1
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 9600
Config Portb = Output
Config Portd.2 = Output
Config Portd.3 = Output
Config Portd.4 = Output
Config Portc = Input
Config Portd.5 = Input
Config Portd.6 = Input
Config Portd.7 = Input

'serial
'PD0: Rx
'PD1: Tx

Dim Lifesignal As Integer , Life As Integer , Send As String * 15
Dim Inn(9) As Byte , A As Byte , A2 As String * 1
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 15 , Input_nr As String * 3
Dim Input_com As String * 1 , Input_ut As String * 2
Dim Input_stat As String * 1 , B As Byte , Led As Byte

Const Id = "002"
Life = 2000

Waitms 4000

Send = Id + ":s:00:1:"
Print Send ; Checksum(send)

Waitms 1000

Top:

Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      Goto Myroutine
   End If

Goto Main

Myroutine:
Select Case Serialchar
Case 48                                                     '0
Goto Set_value
End Select

Main:
'input send off signal
For A = 1 To 9
   If Inn(a) = 1 Then
      Led = 103
      A2 = Str(a)
      If Len(a2) < 2 Then A2 = "0" + A2
      Send = Id + ":i:" + A2 + ":0:"
      Print Send ; Checksum(send)
      End If
Next A

'input send on signal
If Pinc.0 = 0 Then                                          'input 1
   If Inn(1) = 0 Then
   Led = 103
   Send = Id + ":i:01:1:"
   Print Send ; Checksum(send)
   End If
   Inn(1) = 250
End If

If Pinc.1 = 0 Then                                          'input 2
   If Inn(2) = 0 Then
   Led = 103
   Send = Id + ":i:02:1:"
   Print Send ; Checksum(send)
   End If
   Inn(2) = 250
End If

If Pinc.2 = 0 Then                                          'input 3
   If Inn(3) = 0 Then
   Led = 103
   Send = Id + ":i:03:1:"
   Print Send ; Checksum(send)
   End If
   Inn(3) = 250
End If

If Pinc.3 = 0 Then                                          'input 4
   If Inn(4) = 0 Then
   Led = 103
   Send = Id + ":i:04:1:"
   Print Send ; Checksum(send)
   End If
   Inn(4) = 250
End If

If Pinc.4 = 0 Then                                          'input 5
   If Inn(5) = 0 Then
   Led = 103
   Send = Id + ":i:05:1:"
   Print Send ; Checksum(send)
   End If
   Inn(5) = 250
End If

If Pinc.5 = 0 Then                                          'input 6
   If Inn(6) = 0 Then
   Led = 103
   Send = Id + ":i:06:1:"
   Print Send ; Checksum(send)
   End If
   Inn(6) = 250
End If

If Pind.5 = 0 Then                                          'input 7
   If Inn(7) = 0 Then
   Led = 103
   Send = Id + ":i:07:1:"
   Print Send ; Checksum(send)
   End If
   Inn(7) = 250
End If

If Pind.6 = 0 Then                                          'input 8
   If Inn(8) = 0 Then
   Led = 103
   Send = Id + ":i:08:1:"
   Print Send ; Checksum(send)
   End If
   Inn(8) = 250
End If

If Pind.7 = 0 Then                                          'input 9
   If Inn(9) = 0 Then
   Led = 103
   Send = Id + ":i:09:1:"
   Print Send ; Checksum(send)
   End If
   Inn(9) = 250
End If

'set input counters
For A = 1 To 9
   If Inn(a) > 0 And B = 0 Then Decr Inn(a)
Next A

'led timer
If Led > 0 Then Decr Led
If Led = 100 Then Portd.4 = 1
If Led = 0 Then Portd.4 = 0

'lifestring
If Life > 0 Then Decr Life
If Life = 0 Then
   Led = 103
   Send = Id + ":s:01:1:"
   Print Send ; Checksum(send)
   Life = 20000
   End If

'lifesignal
If Lifesignal > 0 Then Decr Lifesignal
If Lifesignal = 500 Then Portd.3 = 1
If Lifesignal = 0 Then
   Portd.3 = 0
   Lifesignal = 2100
   End If

Waitms 1
Goto Top                                                    'loop cycle
End

Set_value:
Input Comminput Noecho                                      'read serialport

Input_nr = Left(comminput , 3)                              'id check
Input_com = Mid(comminput , 5 , 1)                          'command check
Input_ut = Mid(comminput , 7 , 2)                           'output nr check
Input_stat = Mid(comminput , 10 , 1)                        'output stat check

'output
If Input_nr = Id Then

If Input_com = "o" Then
Led = 103
Select Case Input_ut

Case "01"                                                   'output 1
If Input_stat = "1" Then Portb.0 = 1
If Input_stat = "0" Then Portb.0 = 0
Send = Id + ":o:01:" + Str(portb.0) + ":"
Print Send ; Checksum(send)

Case "02"                                                   'output 2
If Input_stat = "1" Then Portb.1 = 1
If Input_stat = "0" Then Portb.1 = 0
Send = Id + ":o:02:" + Str(portb.1) + ":"
Print Send ; Checksum(send)

Case "03"                                                   'output 3
If Input_stat = "1" Then Portb.2 = 1
If Input_stat = "0" Then Portb.2 = 0
Send = Id + ":o:03:" + Str(portb.2) + ":"
Print Send ; Checksum(send)

Case "04"                                                   'output 4
If Input_stat = "1" Then Portb.3 = 1
If Input_stat = "0" Then Portb.3 = 0
Send = Id + ":o:04:" + Str(portb.3) + ":"
Print Send ; Checksum(send)
Case "05"                                                   'output 5
If Input_stat = "1" Then Portb.4 = 1
If Input_stat = "0" Then Portb.4 = 0
Send = Id + ":o:05:" + Str(portb.4) + ":"
Print Send ; Checksum(send)

Case "06"                                                   'output 6
If Input_stat = "1" Then Portb.5 = 1
If Input_stat = "0" Then Portb.5 = 0
Send = Id + ":o:06:" + Str(portb.5) + ":"
Print Send ; Checksum(send)

Case "07"                                                   'output 7
If Input_stat = "1" Then Portb.6 = 1
If Input_stat = "0" Then Portb.6 = 0
Send = Id + ":o:07:" + Str(portb.6) + ":"
Print Send ; Checksum(send)

Case "08"                                                   'output 8
If Input_stat = "1" Then Portb.7 = 1
If Input_stat = "0" Then Portb.7 = 0
Send = Id + ":o:08:" + Str(portb.7) + ":"
Print Send ; Checksum(send)

Case "09"                                                   'output 9
If Input_stat = "1" Then Portd.2 = 1
If Input_stat = "0" Then Portd.2 = 0
Send = Id + ":o:09:" + Str(portd.2) + ":"
Print Send ; Checksum(send)

End Select
End If

If Input_com = "i" Then
Select Case Input_ut

Case "01"
If Inn(1) = 0 Then Inn(1) = 2 Else Inn(1) = 0               'status input 1
Case "02"
If Inn(2) = 0 Then Inn(2) = 2 Else Inn(2) = 0               'status input 2
Case "03"
If Inn(3) = 0 Then Inn(3) = 2 Else Inn(3) = 0               'status input 3
Case "04"
If Inn(4) = 0 Then Inn(4) = 2 Else Inn(4) = 0               'status input 4
Case "05"
If Inn(5) = 0 Then Inn(5) = 2 Else Inn(5) = 0               'status input 5
Case "06"
If Inn(6) = 0 Then Inn(6) = 2 Else Inn(6) = 0               'status input 6
Case "07"
If Inn(7) = 0 Then Inn(7) = 2 Else Inn(7) = 0               'status input 7
Case "08"
If Inn(8) = 0 Then Inn(8) = 2 Else Inn(8) = 0               'status input 8
Case "09"
If Inn(9) = 0 Then Inn(9) = 2 Else Inn(9) = 0               'status input 9

End Select
End If

End If
Goto Main
End
