VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmWireRunWizard 
   Caption         =   "Wire Run Wizard"
   ClientHeight    =   4620
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   6180
   OleObjectBlob   =   "frmWireRunWizard.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "frmWireRunWizard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
' This class represents a form for creating Red and Green sheets.
'
' @Version 1.0
' @Date    31 Aug 2005
' @Author  David Hoyle
'
Option Explicit
Option Compare Text

' This is a constant that defines the SQL statement for returning
' the list of wire runs.
Private Const strWireRunListSQL = _
  "SELECT" & vbLf & _
  "  A.[Construction Unit]," & vbLf & _
  "  A.[Asset ID]," & vbLf & _
  "  A.[Line]," & vbLf & _
  "  A.[Type]," & vbLf & _
  "  C.[Installation Date]," & vbLf & _
  "  C.[Description ID]" & vbLf & _
  "FROM pdbAssets AS A" & vbLf & _
  "  LEFT OUTER JOIN pdbComponentTask AS C ON" & vbLf & _
  "    A.[Route No] = C.[Route No] AND" & vbLf & _
  "    A.[Asset ID] = C.[Asset ID] AND" & vbLf & _
  "    A.[Asset ID] = C.[Section ID] AND" & vbLf & _
  "    C.[Description ID] IN (%param%) AND" & vbLf & _
  "    C.[Status] IN ('S') AND C.[Number] IN (1)" & vbLf & _
  "WHERE A.[Route No] IN (%param%)" & vbLf & _
  "ORDER BY A.[Construction Unit];"

'
' This is the main interface method for this dialogue.
'
' @precon  Must be passed a valid TWireData class to hold the data.
' @postcon Returns the data required to build reand greens.
'
' @param   objWireData as a TWireData
' @return  a Boolean
'
Public Function Execute(ByRef objWireData As TWireData) As Boolean
  Dim i As Long
  Dim dtDate As Date
  Exception.Push "frmWireRunWizard.Execute", objWireData
  On Error GoTo ErrHnd
  Execute = False
  Show
  If mrResult = vbOK Then
    For i = 0 To lbWires.ListCount - 1
      If lbWires.Selected(i) Then
        If IsNull(lbWires.List(i, 5)) Then dtDate = 0 Else dtDate = lbWires.List(i, 5)
        objWireData.AddWireRun _
          cbxRouteNo.Value, _
          lbWires.List(i, 0), _
          lbWires.List(i, 1), _
          lbWires.List(i, 2), _
          lbWires.List(i, 3), _
          dtDate, Nothing
      End If
    Next i
    objWireData.LinesAffected = edtLinesAffected.Value
    objWireData.StartMileage = ConvertToKm(edtStMCh)
    objWireData.StartProtection = ConvertToKm(edtStProtection)
    objWireData.EndMileage = ConvertToKm(edtEndMCh)
    objWireData.EndProtection = ConvertToKm(edtEndProtection)
    objWireData.Worksite = edtStMCh.Value & "+" & edtStProtection.Value & " to " & _
      edtEndMCh.Value & "-" & edtEndProtection.Value
    objWireData.TopMargin = edtTop.Value
    objWireData.TopHeaderMargin = edtTopHeader.Value
    objWireData.LeftMargin = edtLeft.Value
    objWireData.RightMargin = edtRight.Value
    objWireData.BottomMargin = edtBottom.Value
    objWireData.BottomFooterMargin = edtBottomFooter.Value
    If opA4.Value Then objWireData.PaperSize = iA4 Else objWireData.PaperSize = iA3
    If opLandscape.Value Then objWireData.PaperOrientation = iLandscape _
    Else objWireData.PaperOrientation = iPortrait
    For i = 0 To lbElecSect.ListCount - 1
      If lbElecSect.Selected(i) Then
        objWireData.AddElecSect _
          lbElecSect.List(i, 0), _
          lbElecSect.List(i, 1), _
          ConvertToKm(lbElecSect.List(i, 2)), _
          ConvertToKm(lbElecSect.List(i, 3))
      End If
    Next i
    If cbxDEPs.Value Then
      objWireData.DEPStart = ConvertToKm(lbStartDEP.List(lbStartDEP.ListIndex, 1))
      objWireData.DEPStartName = lbStartDEP.List(lbStartDEP.ListIndex, 0)
      objWireData.DEPEnd = ConvertToKm(lbEndDEP.List(lbEndDEP.ListIndex, 1))
      objWireData.DEPEndName = lbEndDEP.List(lbEndDEP.ListIndex, 0)
    Else
      objWireData.DEPStart = 0
      objWireData.DEPEnd = 99999
    End If
    objWireData.Location = edtLocations.Value
    objWireData.Week = edtWeeks.Value
    Execute = True
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Function

'
' This is a button on click event handler for the add button.
'
' @precon  None.
' @postcon Adds an electrical section to the list
'
Private Sub btnAdd_Click()
  Dim strES As String
  Dim strLine As String
  Dim dblStart As Double
  Dim dblEnd As Double
  Dim frm As frmElecSect
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Exception.Push "frmWireRunWizard.btnAdd_Click"
  On Error GoTo ErrHnd
  strES = "AA-0A"
  strLine = "LT"
  dblStart = 0
  dblEnd = 999.99
  Set frm = New frmElecSect
  If frm.Execute(strES, strLine, dblStart, dblEnd) Then
    lbElecSect.AddItem strES
    lbElecSect.Column(1, lbElecSect.ListCount - 1) = strLine
    lbElecSect.Column(2, lbElecSect.ListCount - 1) = Format(dblStart, "0.00")
    lbElecSect.Column(3, lbElecSect.ListCount - 1) = Format(dblEnd, "0.00")
    For i = 0 To lbElecSect.ListCount - 1
      For j = i + 1 To lbElecSect.ListCount - 1
        If lbElecSect.List(i) > lbElecSect.List(j) Then
          For k = 0 To 3
            strES = lbElecSect.List(i, k)
            lbElecSect.List(i, k) = lbElecSect.List(j, k)
            lbElecSect.List(j, k) = strES
          Next k
        End If
      Next j
    Next i
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Exception.Pop
End Sub

'
' This is a button on click event for the Back button.
'
' @precon  None.
' @postcon Allows the user to move backward in the wizard.
'
Private Sub btnPrev_Click()
  Exception.Push "frmWireRunWizard.btnPrev_Click"
  On Error GoTo ErrHnd
  If MultiPage1.Value = 8 And Not chkPageSetup.Value Then
    MultiPage1.Value = MultiPage1.Value - 2
  Else
    MultiPage1.Value = MultiPage1.Value - 1
  End If
  UpdateButtons
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event for the cancel button.
'
' @precon  None.
' @postcon This allow the user to cancel the wizard.
'
Private Sub btnCancel_Click()
  Exception.Push "frmWireRunWizard.btnCancel_Click"
  On Error GoTo ErrHnd
  Reg.Locations.Reload
  Reg.Weeks.Reload
  Reg.LinesAffected.Reload
  Reg.ElectricalSections.Reload
  Reg.DEPs.Reload
  Reg.NonExcludedConfigurations.Reload
  Reg.EMailNames.Reload
  Hide
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event handler for the Delete Electrical section button.
'
' @precon  None.
' @postcon Deletes an electrical section
'
Private Sub btnDelete_Click()
  Dim i As Long
  Exception.Push "frmWireRunWizard.btnDelete_Click"
  On Error GoTo ErrHnd
  For i = lbElecSect.ListCount - 1 To 0 Step -1
    If lbElecSect.Selected(i) Then lbElecSect.RemoveItem i
  Next i
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event handler for the Add DEP button.
'
' @precon  None.
' @postcon Adds a DEP to the lists
'
Private Sub btnDEPAdd_Click()
  Dim frm As frmDEPs
  Dim strSection As String
  Dim dblMileage As Double
  Exception.Push "frmWireRunWizard.btnDEPAdd_Click"
  On Error GoTo ErrHnd
  Set frm = New frmDEPs
  strSection = "G00/00"
  dblMileage = 0#
  If frm.Execute(strSection, dblMileage) Then
    lbStartDEP.AddItem strSection
    lbStartDEP.Column(1, lbStartDEP.ListCount - 1) = Format(dblMileage, "0.00")
    SortLists
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Exception.Pop
End Sub

'
' This sorts and synchonises the DEP lists
'
' @precon  None.
' @postcon Sorts the list of deps.
'
Private Sub SortLists()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim str As String
  Exception.Push "frmWireRunWizard.SortLists"
  On Error GoTo ErrHnd
  For i = 0 To lbElecSect.ListCount - 1
    For j = i + 1 To lbElecSect.ListCount - 1
      If lbElecSect.List(i) > lbElecSect.List(j) Then
        For k = 0 To 3
          str = lbElecSect.List(i, k)
          lbElecSect.List(i, k) = lbElecSect.List(j, k)
          lbElecSect.List(j, k) = str
        Next k
      End If
    Next j
  Next i
  For i = 0 To lbStartDEP.ListCount - 1
    For j = i + 1 To lbStartDEP.ListCount - 1
      If lbStartDEP.List(i) > lbStartDEP.List(j) Then
        For k = 0 To 1
          str = lbStartDEP.List(i, k)
          lbStartDEP.List(i, k) = lbStartDEP.List(j, k)
          lbStartDEP.List(j, k) = str
        Next k
      End If
    Next j
  Next i
  If IsNull(lbStartDEP.List) Then lbEndDEP.Clear Else lbEndDEP.List = lbStartDEP.List
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event handler for the Delete DEP button.
'
' @precon  None.
' @postcon This deletes the selected value from the start DEP list.
'
Private Sub btnDEPDelete_Click()
  Dim frm As frmDEPs
  Exception.Push "frmWireRunWizard.btnDEPDelete_Click"
  On Error GoTo ErrHnd
  If lbStartDEP.ListIndex <> -1 Then
    Set frm = New frmDEPs
    lbStartDEP.RemoveItem lbStartDEP.ListIndex
    SortLists
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Exception.Pop
End Sub

'
' This is a button on click event handler for the Edit DEP button.
'
' @precon  None.
' @postcon This allow the editing of a value in the DEP list.
'
Private Sub btnDEPEdit_Click()
  Dim frm As frmDEPs
  Dim strSection As String
  Dim dblMileage As Double
  Exception.Push "frmWireRunWizard.btnDEPEdit_Click"
  On Error GoTo ErrHnd
  If lbStartDEP.ListIndex <> -1 Then
    Set frm = New frmDEPs
    strSection = lbStartDEP.List(lbStartDEP.ListIndex, 0)
    dblMileage = CDbl(lbStartDEP.List(lbStartDEP.ListIndex, 1))
    If frm.Execute(strSection, dblMileage) Then
      lbStartDEP.Column(0, lbStartDEP.ListIndex) = strSection
      lbStartDEP.Column(1, lbStartDEP.ListIndex) = Format(dblMileage, "0.00")
      SortLists
    End If
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Exception.Pop
End Sub

'
' This is a button on click event handler for the Edit Electrical section button.
'
' @precon  None.
' @postcon Edits an electrical section
'
Private Sub btnEdit_Click()
  Dim strES As String
  Dim strLine As String
  Dim dblStart As Double
  Dim dblEnd As Double
  Dim frm As frmElecSect
  Exception.Push "frmWireRunWizard.btnEdit_Click"
  On Error GoTo ErrHnd
  If lbElecSect.ListIndex <> -1 Then
    strES = lbElecSect.List(lbElecSect.ListIndex, 0)
    strLine = lbElecSect.List(lbElecSect.ListIndex, 1)
    dblStart = CDbl(lbElecSect.List(lbElecSect.ListIndex, 2))
    dblEnd = CDbl(lbElecSect.List(lbElecSect.ListIndex, 3))
    Set frm = New frmElecSect
    If frm.Execute(strES, strLine, dblStart, dblEnd) Then
      lbElecSect.List(lbElecSect.ListIndex, 0) = strES
      lbElecSect.List(lbElecSect.ListIndex, 1) = strLine
      lbElecSect.List(lbElecSect.ListIndex, 2) = Format(dblStart, "0.00")
      lbElecSect.List(lbElecSect.ListIndex, 3) = Format(dblEnd, "0.00")
    End If
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Exception.Pop
End Sub

'
' This is a button on click event for the finish button.
'
' @precon  None.
' @postcon It allow the user to finish the wizard and returns vbOK to the calling
'          procedure via the mrResult variable.
'
Private Sub btnFinish_Click()
  Exception.Push "frmWireRunWizard.btnFinish_Click"
  On Error GoTo ErrHnd
  SaveDialogue
  mrResult = vbOK
  Hide
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a buttons on click event handler for the Next button.
'
' @precon  None.
' @postcon It allow the user to move forward in the wizard.
'
Private Sub btnNext_Click()
  Dim i As Long
  Dim j As Long
  Exception.Push "frmWireRunWizard.btnNext_Click"
  On Error GoTo ErrHnd
  ' Check WorkSite mileages
  If MultiPage1.Value = 1 And cbxWorkSite And (Not IsNumeric(edtStMCh.Text) Or Not IsNumeric(edtEndMCh.Text) Or _
    Not IsNumeric(edtStProtection.Text) Or Not IsNumeric(edtEndProtection.Text)) Then
    MsgBox "Inorder to check for Work Site Mileages the start and end mileages and protections must be valid numbers.", , AppName
    GoTo ErrHnd
  End If
  ' Check Electrical Sections have been select.
  j = 0
  For i = 0 To lbElecSect.ListCount - 1
    If lbElecSect.Selected(i) Then j = j + 1
  Next i
  If MultiPage1.Value = 2 And cbxElecSect And j = 0 Then
    MsgBox "Inorder to check for Electrical Sections you must select at least one Electrical Section from the list.", , AppName
    GoTo ErrHnd
  End If
  ' Check DEPs have been select.
  If MultiPage1.Value = 3 And cbxDEPs And (lbStartDEP.ListIndex = -1 Or lbEndDEP.ListIndex = -1) Then
    MsgBox "Inorder to check for DEPs you must select a Start and End DEP from the two list.", , AppName
    GoTo ErrHnd
  End If
  ' Check that at least one wire run has been selected
  If MultiPage1.Value = 4 Then
    j = 0
    For i = 0 To lbWires.ListCount - 1
      If lbWires.Selected(i) Then j = j + 1
    Next i
    If j = 0 Then
      MsgBox "You must select at least one Asset from the list.", , AppName
      GoTo ErrHnd
    End If
  End If
  If MultiPage1.Value = 6 And Not chkPageSetup.Value Then
    MultiPage1.Value = MultiPage1.Value + 2
  Else
    MultiPage1.Value = MultiPage1.Value + 1
  End If
  UpdateButtons
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' Enables / Disables the chceking of DEPs
'
' @precon  None.
' @postcon Enables or diables the checking of DEPs.
'
Private Sub cbxDEPs_Change()
  Exception.Push "frmWireRunWizard.cbxDEPs_Change"
  On Error GoTo ErrHnd
  frmCheckDEPs.Enabled = cbxDEPs.Value
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' Enabled or disabled the eletrical section checking
'
' @precon  None.
' @postcon Enables or disables the checking of electrical sections.
'
Private Sub cbxElecSect_Change()
  Exception.Push "frmWireRunWizard.cbxElecSect_Change"
  On Error GoTo ErrHnd
  frmElecSect.Enabled = cbxElecSect.Value
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a combo box on change event handler for the route no.
'
' @precon  None.
' @postcon It allow the dataset to be requeried when the route no changes.
'
Private Sub cbxRouteNo_Change()
  Exception.Push "frmWireRunWizard.cbxRouteNo_Change"
  On Error GoTo ErrHnd
  GetWireRunData
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' Enabled or disables the work site mileage box
'
' @precon  None.
' @postcon Enables or disables the checking of work site mileages.
'
Private Sub cbxWorkSite_Change()
  Exception.Push "frmWireRunWizard.cbxWorkSite_Change"
  On Error GoTo ErrHnd
  frmWorkSite.Enabled = cbxWorkSite.Value
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is the forms constructor event handler.
'
' @precon  None.
' @postcon It initialises a database connection and initialises the contents of the
'          dialogue.
'
Private Sub UserForm_Initialize()
  Exception.Push "frmWireRunWizard.UserForm_Initialize"
  On Error GoTo ErrHnd
  mrResult = vbCancel
  UpdateButtons
  InitDialogue
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method updates the status of the dialogues buttons.
'
' @precon  None.
' @postcon the method updates the status of the buttons on the form.
'
Private Sub UpdateButtons()
  Exception.Push "frmWireRunWizard.UpdateButtons"
  On Error GoTo ErrHnd
  btnPrev.Enabled = (MultiPage1.Value <> 0)
  btnNext.Enabled = (MultiPage1.Value <> MultiPage1.Pages.Count - 1)
  btnNext.Default = (MultiPage1.Value <> MultiPage1.Pages.Count - 1)
  btnFinish.Enabled = (MultiPage1.Value = MultiPage1.Pages.Count - 1)
  btnFinish.Default = (MultiPage1.Value = MultiPage1.Pages.Count - 1)
  Caption = "Wire Run Wizard - Step " & MultiPage1.Value + 1 & " of " & MultiPage1.Pages.Count
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method initialises the controls on the dialogue from the registry.
'
' @precon  None.
' @postcon This method initialises the dialogue.
'
Private Sub InitDialogue()
  Dim i As Long
  Dim str As String
  Exception.Push "frmWireRunWizard.InitDialogue"
  On Error GoTo ErrHnd
  For i = 1 To Reg.Locations.Count
    edtLocations.AddItem Reg.Locations.Values(i)
  Next i
  edtLocations.Text = Reg.Location
  For i = 1 To Reg.Weeks.Count
    edtWeeks.AddItem Reg.Weeks.Values(i)
  Next i
  edtWeeks.Text = Reg.Week
  For i = 1 To Reg.LinesAffected.Count
    edtLinesAffected.AddItem Reg.LinesAffected.Values(i)
  Next i
  edtLinesAffected.Text = Reg.LineAffected
  GetRoutes
  cbxRouteNo.Value = Reg.CurrentRoute
  chkCreateSummary.Value = Reg.CreateSummary
  chkHighLight.Value = Reg.HighLightRedAndGreen
  chkHideCols.Value = Reg.HideColumns
  cbxHideComments.Value = Reg.HideComments
  chkTickSheet.Value = Reg.TickSheetHeader
  chkProtect.Value = Reg.Protect
  chkTaskComments.Value = Reg.TaskComment
  cbxSectionComments.Value = Reg.SectionComment
  chkPageSetup.Value = Reg.PageSetup
  cbxSave.Value = Reg.SaveWorkBook
  cbxHideTaskColumns.Value = Reg.HideCompletedTaskCols
  edtTop = Reg.TopMargin
  edtTopHeader = Reg.TopHeader
  edtLeft = Reg.LeftMargin
  edtRight = Reg.RightMargin
  edtBottom = Reg.BottomMargin
  edtBottomFooter = Reg.BottomFooter
  If Reg.PaperSize = iA4 Then opA4 = True Else opA3 = True
  If Reg.PaperOrientation = iLandscape Then opLandscape = True Else opPortrait = True
  cbxWorkSite = Reg.CheckWorksite
  cbxElecSect = Reg.CheckElecSect
  cbxDEPs = Reg.CheckDEPs
  edtStMCh = Reg.StartMileage
  edtStProtection = Reg.StartProtection
  edtEndProtection = Reg.EndProtection
  edtEndMCh = Reg.EndMileage
  cbxWorkSite_Change
  cbxElecSect_Change
  cbxDEPs_Change
  cbxEmails_Change
  For i = 1 To Reg.ElectricalSections.Count
    str = Reg.ElectricalSections.Values(i)
    lbElecSect.AddItem Reg.ElectricalSections.Names(i)
    lbElecSect.Column(1, lbElecSect.ListCount - 1) = GetField(str, 1, Asc(","))
    lbElecSect.Column(2, lbElecSect.ListCount - 1) = Format(CDbl(GetField(str, 2, Asc(","))), "0.00")
    lbElecSect.Column(3, lbElecSect.ListCount - 1) = Format(CDbl(GetField(str, 3, Asc(","))), "0.00")
  Next i
  For i = 1 To Reg.DEPs.Count
    lbStartDEP.AddItem Reg.DEPs.Names(i)
    lbStartDEP.Column(1, lbStartDEP.ListCount - 1) = Reg.DEPs.Values(i)
  Next i
  SortLists
  cbxEmails = Reg.EMailNotifications
  cbxAutoSend = Reg.AutoSend
  For i = 1 To Reg.EMailNames.Count
    lbEMails.AddItem Reg.EMailNames.Values(i)
  Next i
  For i = 0 To lbLinesNotExcluded.ListCount - 1
    lbLinesNotExcluded.Selected(i) = Reg.NonExcludedConfigurations.ReadBool( _
      lbLinesNotExcluded.List(i, 0) & lbLinesNotExcluded.List(i, 1))
  Next i
  chkCreateSummary.Enabled = Not Reg.SingleSheet
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method retreived a list of routes and their names.
'
' @precon  None.
' @postcon Retreived a list of routes and their names.
'
Private Sub GetRoutes()
  Const strSQL = "SELECT * FROM pdbRoutes;"
  Dim rs As Recordset
  Exception.Push "frmWireRunWizard.GetRoutes"
  On Error GoTo ErrHnd
  cbxRouteNo.Clear
  Set rs = ds.OpenRecordSet(strSQL)
  While Not rs.EOF
    cbxRouteNo.AddItem rs.Fields("Route No").Value
    cbxRouteNo.Column(1, cbxRouteNo.ListCount - 1) = rs.Fields("Description").Value
    rs.MoveNext
  Wend
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  ds.CloseRecordSet
  Exception.Pop
End Sub

'
' This method gets the wire run data from the database based on the route number.
'
' @precon  None.
' @postcon This method retrieves the wire run data from the database.
'
Private Sub GetWireRunData()
  Dim rs As Recordset
  Dim i As Long
  Exception.Push "frmWireRunWizard.GetWireRunData"
  On Error GoTo ErrHnd
  lbWires.Clear
  Set rs = ds.OpenRecordSet(FormatString(strWireRunListSQL, _
    Reg.WiringTasks.List(True, False), CInt(cbxRouteNo.Text)))
  While Not rs.EOF
    lbWires.AddItem rs.Fields("Asset ID").Value
    i = lbWires.ListCount - 1
    lbWires.Column(1, i) = rs.Fields("Construction Unit").Value
    lbWires.Column(2, i) = rs.Fields("Line").Value
    lbWires.Column(3, i) = rs.Fields("Type").Value
    If Not IsNull(rs.Fields("Description ID").Value) Then
      If Not IsNull(rs.Fields("Installation Date").Value) Then
        lbWires.Column(4, i) = Format(rs.Fields("Installation Date").Value, "ddd dd/mmm/yyyy")
        lbWires.Column(5, i) = Format(rs.Fields("Installation Date").Value, "dd/mmm/yyyy")
      Else
        lbWires.Column(4, i) = "(Not Installed)"
        lbWires.Column(5, i) = 0
      End If
    End If
    AddLineAndType rs.Fields("Line").Value, rs.Fields("Type").Value
    rs.MoveNext
  Wend
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  ds.CloseRecordSet
  Exception.Pop
End Sub

'
' This method saves the contents of the dialogue to the registry.
'
' @precon  None.
' @postcon Saves the settings changed on the form for the registry object.
'
Private Sub SaveDialogue()
  Dim i As Long
  Exception.Push "frmWireRunWizard.SaveDialogue"
  On Error GoTo ErrHnd
  AddToComboBox edtLocations
  For i = 0 To edtLocations.ListCount - 1
    Reg.Locations.WriteString edtLocations.List(i), edtLocations.List(i)
  Next i
  Reg.Locations.Save
  Reg.Location = edtLocations.Text
  AddToComboBox edtWeeks
  For i = 0 To edtWeeks.ListCount - 1
    Reg.Weeks.WriteString edtWeeks.List(i), edtWeeks.List(i)
  Next i
  Reg.Weeks.Save
  Reg.Week = edtWeeks.Text
  AddToComboBox edtLinesAffected
  For i = 0 To edtLinesAffected.ListCount - 1
    Reg.LinesAffected.WriteString edtLinesAffected.List(i), edtLinesAffected.List(i)
  Next i
  Reg.LinesAffected.Save
  Reg.LineAffected = edtLinesAffected.Text
  Reg.CurrentRoute = cbxRouteNo.Text
  Reg.CreateSummary = chkCreateSummary.Value
  Reg.HighLightRedAndGreen = chkHighLight.Value
  Reg.HideColumns = chkHideCols.Value
  Reg.HideComments = cbxHideComments.Value
  Reg.TickSheetHeader = chkTickSheet.Value
  Reg.Protect = chkProtect.Value
  Reg.TaskComment = chkTaskComments.Value
  Reg.SectionComment = cbxSectionComments.Value
  Reg.PageSetup = chkPageSetup.Value
  Reg.SaveWorkBook = cbxSave.Value
  Reg.HideCompletedTaskCols = cbxHideTaskColumns.Value
  Reg.TopMargin = edtTop
  Reg.TopHeader = edtTopHeader
  Reg.LeftMargin = edtLeft
  Reg.RightMargin = edtRight
  Reg.BottomMargin = edtBottom
  Reg.BottomFooter = edtBottomFooter
  If opA4 Then Reg.PaperSize = iA4 Else Reg.PaperSize = iA3
  If opLandscape Then Reg.PaperOrientation = iLandscape Else Reg.PaperSize = iPortrait
  Reg.CheckWorksite = cbxWorkSite
  Reg.CheckElecSect = cbxElecSect
  Reg.CheckDEPs = cbxDEPs
  Reg.StartMileage = edtStMCh
  Reg.StartProtection = edtStProtection
  Reg.EndMileage = edtEndMCh
  Reg.EndProtection = edtEndProtection
  Reg.ElectricalSections.Reset
  For i = 0 To lbElecSect.ListCount - 1
    Reg.ElectricalSections.WriteString lbElecSect.List(i, 0), _
      FormatString("%param%,%param%,%param%", lbElecSect.List(i, 1), lbElecSect.List(i, 2), lbElecSect.List(i, 3))
  Next i
  Reg.ElectricalSections.Save
  Reg.DEPs.Reset
  For i = 0 To lbStartDEP.ListCount - 1
    Reg.DEPs.WriteString lbStartDEP.List(i, 0), lbStartDEP.List(i, 1)
  Next i
  Reg.DEPs.Save
  Reg.EMailNotifications = cbxEmails
  Reg.AutoSend = cbxAutoSend
  Reg.EMailNames.Reset
  For i = 1 To lbEMails.ListCount
    Reg.EMailNames.WriteString lbEMails.List(i - 1), lbEMails.List(i - 1)
  Next i
  Reg.EMailNames.Save
  Reg.NonExcludedConfigurations.Reset
  For i = 0 To lbLinesNotExcluded.ListCount - 1
    Reg.NonExcludedConfigurations.WriteString lbLinesNotExcluded.List(i, 0) & _
      lbLinesNotExcluded.List(i, 1), lbLinesNotExcluded.Selected(i)
  Next i
  Reg.NonExcludedConfigurations.Save
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method addes the current combo box text to the list if its neither null or already exists.
' This maintains a sorted list
'
' @precon  None.
' @postcon This method addes the current combo box text to the list if its neither null
'          or already exists and maintains a sorted list
'
' @param   cbx as a ComboBox
'
Private Sub AddToComboBox(cbx As ComboBox)
  Dim i As Long
  Dim j As Long
  Exception.Push "frmWireRunWizard.AddToComboBox", cbx
  On Error GoTo ErrHnd
  If cbx.Text = "" Then GoTo ErrHnd
  For i = 0 To cbx.ListCount - 1
    j = i
    If cbx.Text <= cbx.List(i) Then
      If Not cbx.Text = cbx.List(i) Then cbx.AddItem cbx.Text, j
      GoTo ErrHnd
    End If
  Next i
  cbx.AddItem cbx.Text
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event handler for the add email button.
'
' @precon  None.
' @postcon Adds an email name to the list
'
Private Sub btnAddEMail_Click()
  Dim str As String
  Exception.Push "frmWireRunWizard.btnAddEmail_Click"
  On Error GoTo ErrHnd
  str = InputBox("Please enter an email name:", AppName, "")
  If str <> "" Then lbEMails.AddItem str
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This is a button on click event handler fo the delete email button.
'
' @precon  None.
' @postcon Deletes an email name from the list.
'
Private Sub btnDeleteEMail_Click()
  Exception.Push "frmWireRunWizard.btnDeleteEMail_Click"
  On Error GoTo ErrHnd
  If lbEMails.ListIndex <> -1 Then lbEMails.RemoveItem lbEMails.ListIndex
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' Enabled / Disable the EMail notification area
'
' @precon  None.
' @postcon Enables or disables the notification of emails.
'
Private Sub cbxEmails_Change()
  Exception.Push "frmWireRunWizard.cbxEMails_Change"
  On Error GoTo ErrHnd
  frmEmails.Enabled = cbxEmails.Value
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method adds the line and type to the list is its not already in the list.
'
' @precon  None.
' @postcon Adds the line and type to the list is its not already in the list.
'
' @param   strLine as a String
' @param   strType as a String
'
Private Sub AddLineAndType(strLine As String, strType As String)
  Dim i As Long
  Dim boolFound As Boolean
  Exception.Push "frmWireRunWizard.AddLineAndType", strLine, strType
  On Error GoTo ErrHnd
  boolFound = False
  For i = 0 To lbLinesNotExcluded.ListCount - 1
    If strLine = lbLinesNotExcluded.List(i, 0) And strType = lbLinesNotExcluded.List(i, 1) Then
      boolFound = True
      Exit For
    End If
  Next i
  If Not boolFound Then
    lbLinesNotExcluded.AddItem strLine
    lbLinesNotExcluded.Column(1, lbLinesNotExcluded.ListCount - 1) = strType
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub
