VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "TRedAndGreen"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'
' This class creates the red and green sheets
'
' @Version 1.0
' @Date    05 Sep 2005
' @Author  David Hoyle
'
Option Explicit
Option Compare Text

' A constant to define the column width of a matrix item
Private Const dblColWidth As Double = 6.5

' This is a reference for a worksheet
Private Sh As Worksheet
' This is a data matrix containing all the information for the red and green
Private DataMatrix As TDataMatrix
' A ADODB record set for the class
Private rs As ADODB.Recordset
' A record to contain wire run data
Private WireData As TWireData
' A location of the gates
Private iGates As Long
' An array of sheet names
Private strSheets() As String
' An unchanged internal list of the Cat Ids for the Red and Green.
Private FCatIDs As String
' A private variable to define the current progress through the creation of the R&Gs
Private iProgress As Long

'
' This is the main pubilc method that is used to execute the class.
'
' @precon  None.
' @postcon Displays the Wire Run Wizard form and processes the results.
'
Public Sub Execute()
  Dim frm As frmWireRunWizard
  Dim i As Long
  Dim iCounter As Long
  Dim iRefStyle As Long
  Exception.Push "TRedAndGreen.Execute"
  On Error GoTo ErrHnd
  If Reg.CatID.List(True, False) <> "" Then
    If Reg.Statuses.List(True, False) <> "" Then
      iRefStyle = Application.ReferenceStyle
      Application.ReferenceStyle = xlA1
      Set frm = New frmWireRunWizard
      Set WSRpt = New TWorkCheckReport
      ReDim strSheets(0 To 0) As String
      Set WireData = New TWireData
      If frm.Execute(WireData) Then
        Progress.Show 0, WireData.WireRunCount
        For i = WireData.WireRunCount To 1 Step -1
          iProgress = WireData.WireRunCount - i
          iCounter = iCounter + 1
          Application.ScreenUpdating = False
          If GetDataSet(i) Then OutputMatrixData i
          Application.ScreenUpdating = True
          DoEvents
        Next i
        CreateSummary WireData
        ReportOnWorkSiteProblems
        SaveWorkBook
        EMailNotifications
      End If
    Else
      MsgBox "You must select at least one Status within the options before you can " & _
        "create a Red and Green sheet.", , AppName
    End If
  Else
    MsgBox "You must select at least one Category within the options before you can " & _
      "create a Red and Green sheet.", , AppName
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Application.ReferenceStyle = iRefStyle
  Set WSRpt = Nothing
  If Not frm Is Nothing Then Unload frm
  Set frm = Nothing
  Progress.Hide
  If Not Application.ScreenUpdating Then Application.ScreenUpdating = True
  If Not DataMatrix Is Nothing Then Set DataMatrix = Nothing
  Exception.Pop
End Sub

'
' This method creates a summary from the wire runs selected is the summary option is enabled.
'
' @precon  None.
' @postcon Creates a summary from the wire runs selected is the summary option is enabled.
'
' @param   WireData as a TWireData
'
Private Sub CreateSummary(WireData As TWireData)
  Dim Summary As TSummary
  Exception.Push "TRedAndGreen.CreateSummary", WireData
  On Error GoTo ErrHnd
  If Reg.CreateSummary Then
    Set Summary = New TSummary
    Summary.Execute strSheets, WireData
    Set Summary = Nothing
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method saves the current workbook if the setting is enabled.
'
' @precon  None.
' @postcon Saves the current workbook if the setting is enabled.
'
Private Sub SaveWorkBook()
  Dim strFileName As String
  Dim strPassword As String
  Exception.Push "TRedAndGreen.SaveWorkBook"
  On Error GoTo ErrHnd
  If Reg.SaveWorkBook Then
    strFileName = Application.GetSaveAsFilename(ActiveWorkbook.Name, "Excel Workbooks (*.xls),*.xls", 1, AppName, "Save")
    While Dir$(strFileName) <> ""
      MsgBox "The file name of the workbook can not be the same as the existing file """ & strFileName & """."
      strFileName = Application.GetSaveAsFilename(ActiveWorkbook.Name, "Excel Workbooks (*.xls),*.xls", 1, AppName, "Save")
    Wend
    If strFileName <> "False" Then
      strPassword = InputBox("Please enter a password:", AppName, "")
      ActiveWorkbook.SaveAs strFileName, , , strPassword, , True, , , True
    End If
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method notifies people via email of the changes and problem on the red and greens is the
' option is enabled.
'
' @precon  None.
' @postcon Notifies people via email of the changes and problem on the red and greens is the
'          option is enabled.
'
Private Sub EMailNotifications()
  Dim myItem As Variant
  Dim str As String
  Dim strPrefix As String
  Exception.Push "TRedAndGreen.EMailNotifications"
  On Error GoTo ErrHnd
  If Reg.EMailNotifications Then
    Set myItem = CreateObject("Outlook.Application").CreateItem(0)
    myItem.To = GetAddresses
    str = GetProblems
    If str <> "" Then
      myItem.HTMLBody = _
        strHTMLStyles & _
        strHTMLBodyStart & _
        str & _
        "</TABLE>"
      strPrefix = "ACTION REQUIRED: "
    Else
      strPrefix = "For Information: "
    End If
    myItem.Subject = strPrefix & ActiveWorkbook.Path & "\" & ActiveWorkbook.Name & " - Created"
    myItem.Display
    If Reg.AutoSend Then myItem.Send
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method retreives the data set for the wire run specified
'
' @precon  None.
' @postcon Retreives the data set for the wire run specified
'
' @param   iIndex as a Long
' @return  a Boolean
'
Private Function GetDataSet(iIndex As Long) As Boolean
  Const iComma As Long = 44
  Exception.Push "TRedAndGreen.GetDataSet", iIndex
  On Error GoTo ErrHnd
  GetDataSet = False
  Progress.Progress "Getting data set...", iProgress
  Set rs = ds.OpenRecordSet(FormatString(strMatrixSQL, Reg.StaggerCategory, _
    Reg.StaggerID, WireData.RouteNo(iIndex), WireData.AssetID(iIndex), _
    Reg.CatID.List(True, False, Chr$(iComma)), Reg.Statuses.List(True, True, Chr$(iComma))))
  GetDataSet = True
  If GetDataSet Then GetDataMatrix WireData.InstallDate(iIndex)
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  ds.CloseRecordSet
  Exception.Pop
End Function

'
' This method takes the data set and transfer the data into the matrix structrure.
'
' @precon  Recordset must be open.
' @postcon Takes the data set and transfer the data into the matrix structrure.
'
' @param   dtDate as a date
'
Private Sub GetDataMatrix(dtDate As Date)
  Exception.Push "TRedAndGreen.GetDataMatrix", dtDate
  On Error GoTo ErrHnd
  Progress.Progress "Analysing data...", iProgress
  Set DataMatrix = New TDataMatrix
  While Not rs.EOF
    DataMatrix.Add _
      rs.Fields("Route No").Value, _
      rs.Fields("Asset ID").Value, _
      rs.Fields("Section ID").Value, _
      rs.Fields("Construction Unit").Value, _
      rs.Fields("Line").Value, _
      rs.Fields("Type").Value, _
      rs.Fields("Section Description").Value, _
      rs.Fields("Design Stagger").Value, _
      rs.Fields("Description ID").Value, _
      rs.Fields("Status").Value, _
      rs.Fields("Number").Value, _
      rs.Fields("When ID").Value, _
      rs.Fields("Task Description").Value, _
      rs.Fields("Default Time").Value, _
      Not IsNull(rs.Fields("Installation Date").Value), _
      rs.Fields("Comment").Value & "", _
      rs.Fields("Cat ID").Value, _
      rs.Fields("Start Location").Value, _
      rs.Fields("Section Comment").Value & "", _
      rs.Fields("Default Task Exclusion").Value, _
      rs.Fields("Requires Materials").Value, _
      rs.Fields("Required for Contact Wire").Value, _
      rs.Fields("Dropper Task").Value
    rs.MoveNext
  Wend
  DataMatrix.InstallDate = dtDate
  DataMatrix.Sort
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the matrix data onto a new sheet
'
' @precon  None.
' @postcon Outputs the matrix data onto a new sheet
'
' @param   iIndex as a Long
'
Private Sub OutputMatrixData(iIndex As Long)
  Exception.Push "TRedAndGreen.OutputMatrixData", iIndex
  On Error GoTo ErrHnd
  Set Sh = GetNewWorksheet(WireData.AssetID(iIndex), True, False)
  With Sh.Outline
    .AutomaticStyles = False
    .SummaryRow = xlAbove
    .SummaryColumn = xlLeft
  End With
  ReDim Preserve strSheets(0 To UBound(strSheets) + 1)
  strSheets(UBound(strSheets)) = Sh.Name
  BookKeepingInfo iIndex
  ' Wire Run Info
  With WireData
    Sh.Cells(iRowTaskDesc, iColDescription) = WireData.CU(iIndex) & " - " & _
      WireData.AssetID(iIndex) & vbLf & "(" & WireData.AssetLine(iIndex) & "  " & _
      WireData.AssetType(iIndex) & ")"
      Sh.Cells(iRowTaskDesc, iColDescription).Font.Bold = True
      Sh.Cells(iRowTaskDesc, iColDescription).Font.Size = 18
      Sh.Cells(iRowTaskDesc, iColDescription).HorizontalAlignment = xlHAlignCenter
      Sh.Cells(iRowTaskDesc, iColDescription).VerticalAlignment = xlVAlignCenter
  End With
  OutputColHeader iIndex
  OutputColumns iIndex
  OutputRowHeader
  OutputRows
  OutputDataArea
  OutputColTotals
  OutputRowTotals
  OutputOverAllTotals
  OutputGates
  OutputCatIDColumns
  OutputComments
  HideCompletedTaskColumns Reg.HideCompletedTaskCols
  If Reg.PageSetup Then PageSetup
  Sh.Cells(iRowDataStart, iColDataStart).Select
  ActiveWindow.FreezePanes = True
  Sh.Columns(iColDataStart).Hidden = True
  Sh.Rows(iRowDataStart).Hidden = True
  Sh.Columns(iColDataStart + DataMatrix.TaskCount + 1).Hidden = True
  Sh.Rows(iRowDataStart + DataMatrix.SectionCount + 1).Hidden = True
  Sh.Columns(iColCU).Hidden = True
  Sh.Columns(iColLine).Hidden = True
  Sh.Columns(iColType).Hidden = True
  If Reg.Protect Then Sh.Protect ""
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method hides the completed task columns and show the uncompleted task columns
' in the data matrix area.
'
' @precon  None.
' @postcon Hides the completed task columns and show the uncompleted task columns
'          in the data matrix area.
'
' @param   boolHide as a Boolean
'
Private Sub HideCompletedTaskColumns(boolHide As Boolean)
  Dim dbl As Double
  Dim iTask As Long
  Exception.Push "TRedAndGreen.HideCompletedTaskColumns", boolHide
  On Error GoTo ErrHnd
  For iTask = 1 To DataMatrix.TaskCount
    dbl = Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1 + 3, iColDataStart + iTask).Value
    Sh.Columns(iColDataStart + iTask).Hidden = ((dbl = 0) And boolHide)
  Next iTask
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method logs book keeping information using references so that the updater can use this
' later.
'
' @precon  None.
' @postcon Logs book keeping information using references so that the updater can use this
'          later.
'
' @param   iIndex as a Long
'
Private Sub BookKeepingInfo(iIndex As Long)
  Exception.Push "TRedAndGreen.BookKeepingInfo", iIndex
  On Error GoTo ErrHnd
  Progress.Progress "Logging book keeping information...", iProgress
  Sh.Cells(iBKVersion, iColRouteNo).Value = "Version : "
  Sh.Cells(iBKVersion, iColDescription).NumberFormat = "0.0"
  Sh.Cells(iBKVersion, iColDescription) = CDbl(iMajorVersion & "." & iMinorVersion)
  Sh.Cells(iBKOutputStyle, iColRouteNo).Value = "Style : "
  If Reg.SingleSheet Then
    Sh.Cells(iBKOutputStyle, iColDescription).Value = "Single"
  Else
    Sh.Cells(iBKOutputStyle, iColDescription).Value = "Multiple"
  End If
  Sh.Cells(iBKType, iColRouteNo).Value = "Type : "
  Sh.Cells(iBKType, iColDescription).Value = "R&G"
  Sh.Cells(iBKRoute, iColRouteNo).Value = "Route : "
  Sh.Cells(iBKRoute, iColDescription).Value = WireData.RouteNo(iIndex)
  Sh.Cells(iBKWireID, iColRouteNo).Value = "Asset ID : "
  Sh.Cells(iBKWireID, iColDescription).Value = WireData.AssetID(iIndex)
  Sh.Cells(iBKCU, iColRouteNo).Value = "CU : "
  Sh.Cells(iBKCU, iColDescription) = WireData.CU(iIndex)
  Sh.Cells(iBKAssetLine, iColRouteNo).Value = "Line : "
  Sh.Cells(iBKAssetLine, iColDescription) = WireData.AssetLine(iIndex)
  Sh.Cells(iBKAssetType, iColRouteNo).Value = "Type : "
  Sh.Cells(iBKAssetType, iColDescription) = WireData.AssetType(iIndex)
  Sh.Cells(iBKCatIDs, iColRouteNo).Value = "Cat IDs : "
  Sh.Cells(iBKCatIDs, iColDescription) = Reg.CatID.List(True, False)
  Sh.Cells(iBKStatusIDs, iColRouteNo).Value = "Status IDs : "
  Sh.Cells(iBKStatusIDs, iColDescription) = Reg.Statuses.List(True, True)
  Sh.Cells(iBKCreated, iColRouteNo) = "Created : "
  Sh.Cells(iBKCreated, iColRouteNo).Font.Bold = True
  Sh.Cells(iBKCreated, iColDescription).NumberFormat = "ddd dd/mmm/yyyy ""@"" hh:mm:ss"
  Sh.Cells(iBKCreated, iColDescription) = Now
  Sh.Cells(iBKUpdated, iColRouteNo) = "Updated : "
  Sh.Cells(iBKUpdated, iColRouteNo).Font.Bold = True
  Sh.Cells(iBKUpdated, iColDescription).NumberFormat = "ddd dd/mmm/yyyy ""@"" hh:mm:ss"
  Rows(iBKVersion & ":" & iBKStatusIDs).Hidden = True
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the column header information for the tasks
'
' @precon  None.
' @postcon Outputs the column header information for the tasks
'
' @param   iIndex as a Long
'
Private Sub OutputColHeader(iIndex As Long)
  Exception.Push "TRedAndGreen.OutputColHeader", iIndex
  On Error GoTo ErrHnd
  Progress.Progress "Outputting column header...", iProgress
  ' Titles
  Sh.Cells(iRowNumber, iColExclusions) = "Number"
  Sh.Cells(iRowNumber, iColExclusions).Font.Bold = True
  ' Titles
  Sh.Cells(iRowStatus, iColExclusions) = "Status"
  Sh.Cells(iRowStatus, iColExclusions).Font.Bold = True
  ' Titles
  Sh.Cells(iRowWhenID, iColExclusions) = "When ID"
  Sh.Cells(iRowWhenID, iColExclusions).Font.Bold = True
  ' Titles
  Sh.Cells(iRowCatID, iColExclusions) = "Cat ID"
  Sh.Cells(iRowCatID, iColExclusions).Font.Bold = True
  ' Titles
  Sh.Cells(iRowTaskMats, iColExclusions) = "Mats"
  Sh.Cells(iRowTaskMats, iColExclusions).Font.Bold = True
  ' Data Area Start and End Markers
  Sh.Cells(iRowTaskDesc, iColDataStart) = "#DataColumnStart#"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 1) = "#DataColumnEnd#"
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the task information in the columns of the Red and Green.
'
' @precon  None.
' @postcon Outputs the task information in the columns of the Red and Green.
'
' @param   iIndex as a Long
'
Private Sub OutputColumns(iIndex As Long)
  Dim iTask As Long
  Dim strYesNo(-1 To 0) As String
  Exception.Push "TRedAndGreen.OutputColumns", iIndex
  On Error GoTo ErrHnd
  strYesNo(-1) = "Yes"
  strYesNo(0) = "No"
  ' Output task area
  For iTask = 1 To DataMatrix.TaskCount
    Sh.Cells(iRowNumber, iTask + iColDataStart).Value = DataMatrix.TaskNumber(iTask)
    Sh.Cells(iRowStatus, iTask + iColDataStart).Value = DataMatrix.TaskStatus(iTask)
    ' When ID Output and Colour
    Sh.Cells(iRowWhenID, iTask + iColDataStart).Value = DataMatrix.TaskWhenID(iTask)
    Sh.Cells(iRowWhenID, iTask + iColDataStart).Interior.ColorIndex = _
      ItemColourIndex(DataMatrix.TaskWhenID(iTask))
    ' Cat ID Output and Colour
    Sh.Cells(iRowCatID, iTask + iColDataStart).Value = DataMatrix.TaskCatID(iTask)
    Sh.Cells(iRowCatID, iTask + iColDataStart).Interior.ColorIndex = _
      ItemColourIndex(DataMatrix.TaskCatID(iTask), FCatIDs)
    Sh.Cells(iRowTaskDesc, iTask + iColDataStart).Value = DataMatrix.TaskDescription(iTask)
    Sh.Cells(iRowTaskID, iTask + iColDataStart).Value = DataMatrix.TaskID(iTask)
    If (DataMatrix.WiringTask(iTask) And (WireData.InstallDate(iIndex) > 0)) _
      Or Reg.NonExcludedConfigurations.ReadBool(WireData.AssetLine(iIndex) & WireData.AssetType(iIndex)) Then
      Sh.Cells(iRowTaskExc, iTask + iColDataStart).Value = "No"
    Else
      Sh.Cells(iRowTaskExc, iTask + iColDataStart).Value = strYesNo(DataMatrix.DefaultExclusion(iTask))
    End If
    Sh.Cells(iRowTaskMats, iTask + iColDataStart).Value = strYesNo(DataMatrix.RequiresMaterials(iTask))
  Next iTask
  FormatColumnHeader
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method formats the column header for the Red and Green sheet.
'
' @precon  None.
' @postcon Formats the column header for the Red and Green sheet.
'
Private Sub FormatColumnHeader()
  Exception.Push "TRedAndGreen.FormatColumnHeader"
  On Error GoTo ErrHnd
  ' Format Numbers
  Range(Sh.Cells(iRowNumber, iColDataStart), Sh.Cells(iRowNumber, iColDataStart + _
    DataMatrix.TaskCount + 1)).HorizontalAlignment = xlHAlignCenter
  ' Format Status IDs
  Range(Sh.Cells(iRowStatus, iColDataStart), Sh.Cells(iRowStatus, iColDataStart + _
    DataMatrix.TaskCount + 1)).HorizontalAlignment = xlHAlignCenter
  ' Format When IDs
  Range(Sh.Cells(iRowWhenID, iColDataStart), Sh.Cells(iRowWhenID, iColDataStart + _
    DataMatrix.TaskCount + 1)).HorizontalAlignment = xlHAlignCenter
  ' Format Cat IDs
  Range(Sh.Cells(iRowCatID, iColDataStart), Sh.Cells(iRowCatID, iColDataStart + _
    DataMatrix.TaskCount + 1)).HorizontalAlignment = xlHAlignCenter
  ' Format Cat IDs
  With Range(Sh.Cells(iRowTaskDesc, iColDataStart), Sh.Cells(iRowTaskDesc, _
    iColDataStart + DataMatrix.TaskCount + 1))
    .HorizontalAlignment = xlHAlignCenter
    .Orientation = xlUpward
  End With
  ' Format Task IDs
  Range(Sh.Cells(iRowTaskID, iColDataStart), Sh.Cells(iRowTaskID, iColDataStart + _
    DataMatrix.TaskCount + 1)).HorizontalAlignment = xlHAlignCenter
  ' Format Task Decriptions
  With Sh.Rows(iRowTaskDesc)
    .WrapText = True
    .RowHeight = 150
  End With
  ConditionalFormatColumnHeader
  ' Border matrix area
  Range(Sh.Cells(iRowTaskExc, iColDataStart), Sh.Cells(iRowTaskExc, iColDataStart + _
    DataMatrix.TaskCount + 1)).Borders(xlEdgeBottom).Weight = xlMedium
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method Conditionally formats the column header for the Red and Green sheet.
'
' @precon  None.
' @postcon Conditionally formats the column header for the Red and Green sheet.
'
Private Sub ConditionalFormatColumnHeader()
  Exception.Push "TRedAndGreen.ConditionalFormatColumnHeader"
  On Error GoTo ErrHnd
  ' Size and Hide Columns
  With Range(Sh.Cells(iRowNumber, iColDataStart - 1), Sh.Cells(iRowTaskMats, iColDataStart + _
    DataMatrix.TaskCount))
    .Columns.ColumnWidth = dblColWidth
    .FormatConditions.Add(xlExpression, , "=A$" & iRowTaskExc & "=""Yes""").Interior.Color = 12632256
  End With
  ' Format Materials
  With Range(Sh.Cells(iRowTaskMats, iColDataStart - 1), Sh.Cells(iRowTaskMats, iColDataStart + _
    DataMatrix.TaskCount))
    .HorizontalAlignment = xlHAlignCenter
    .Locked = False
    .FormatConditions.Add(xlCellValue, xlEqual, "Yes").Font.Color = 255
    .FormatConditions.Add(xlCellValue, xlEqual, "No").Font.Color = 16711680
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Tasks with Materials"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether this task has materials."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
  End With
  ' Format Exclusions
  With Range(Sh.Cells(iRowTaskExc, iColDataStart - 1), Sh.Cells(iRowTaskExc, iColDataStart + _
    DataMatrix.TaskCount))
    .HorizontalAlignment = xlHAlignCenter
    .Locked = False
    With .FormatConditions.Add(xlCellValue, xlEqual, "Yes")
      .Font.Color = 255
      .Interior.Color = 12632256
    End With
    .FormatConditions.Add(xlCellValue, xlEqual, "No").Font.Color = 16711680
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Tasks Exclusions"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether this task should be excluded."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the row header information for the Sections.
'
' @precon  None.
' @postcon Outputs the row header information for the Sections.
'
'
Private Sub OutputRowHeader()
  Exception.Push "TRedAndGreen.OutputRowHeader"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting row header...", iProgress
  ' Titles
  Sh.Cells(iRowTaskExc, iColRouteNo) = "Route No"
  Sh.Cells(iRowTaskExc, iColAssetID) = "Asset ID"
  Sh.Cells(iRowTaskExc, iColSectID) = "Section ID"
  Sh.Cells(iRowTaskExc, iColCU) = "CU"
  Sh.Cells(iRowTaskExc, iColLine) = "Line"
  Sh.Cells(iRowTaskExc, iColType) = "Type"
  Sh.Cells(iRowTaskExc, iColDescription) = "Section Description"
  Sh.Cells(iRowTaskExc, iColStagger) = "    Stagger"
  Sh.Cells(iRowTaskExc, iColLocation) = "Loc (Km)"
  Sh.Cells(iRowTaskExc, iColExclusions) = "Excl"
  Range(Sh.Cells(iRowTaskExc, iColRouteNo), _
  Sh.Cells(iRowTaskExc, iColExclusions)).Font.Bold = True
  With Sh.Cells(iRowTaskID, iColDescription)
    .Interior.ColorIndex = 36
    .Font.Bold = True
    .Value = "Highlight Red and Green:"
    .Characters(11, 3).Font.ColorIndex = 3
    .Characters(19, 5).Font.ColorIndex = 10
    .HorizontalAlignment = xlHAlignRight
  End With
  With Sh.Cells(iRowTaskID, iColStagger)
    .Interior.ColorIndex = 36
    .Font.Bold = True
    .HorizontalAlignment = xlHAlignCenter
    .Value = "No"
    If Reg.HighLightRedAndGreen Then .Value = "Yes"
    .FormatConditions.Add(xlCellValue, xlEqual, "Yes").Font.ColorIndex = 5
    .FormatConditions.Add(xlCellValue, xlEqual, "No").Font.ColorIndex = 3
    .Locked = False
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Hightlight Red and Green"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether this should be highlighted Red and Green."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
  End With
  ' Data Area Start and End Markers
  Sh.Cells(iRowDataStart, iColDescription) = "#DataRowStart#"
  Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iColDescription) = "#DataRowEnd#"
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub
  
'
' This method outputs the section information in the red and green sheet.
'
' @precon  None.
' @postcon Outputs the section information in the red and green sheet.
'
Private Sub OutputRows()
  Dim iSection As Long
  Exception.Push "TRedAndGreen.OutputRows"
  On Error GoTo ErrHnd
  ' Output Section Info
  For iSection = 1 To DataMatrix.SectionCount
    Sh.Cells(iRowDataStart + iSection, iColRouteNo).Value = DataMatrix.RouteNo(iSection)
    Sh.Cells(iRowDataStart + iSection, iColAssetID).Value = DataMatrix.AssetID(iSection)
    Sh.Cells(iRowDataStart + iSection, iColSectID).Value = DataMatrix.SectionID(iSection)
    Sh.Cells(iRowDataStart + iSection, iColCU).Value = DataMatrix.CU(iSection)
    Sh.Cells(iRowDataStart + iSection, iColLine).Value = DataMatrix.AssetLine(iSection)
    Sh.Cells(iRowDataStart + iSection, iColType).Value = DataMatrix.AssetType(iSection)
    Sh.Cells(iRowDataStart + iSection, iColDescription).Value = DataMatrix.Description(iSection)
    Sh.Cells(iRowDataStart + iSection, iColStagger).Value = DataMatrix.Stagger(iSection)
    Sh.Cells(iRowDataStart + iSection, iColLocation).Value = DataMatrix.Location(iSection)
    Sh.Cells(iRowDataStart + iSection, iColExclusions).Value = "No"
    If Reg.CheckWorksite Then CheckWorksite iSection, DataMatrix, WireData, Sh
    If Reg.CheckElecSect Then CheckElectricalSections iSection, DataMatrix, WireData, Sh
    If Reg.CheckDEPs Then CheckDEPs iSection, DataMatrix, WireData, Sh
  Next iSection
  FormatRowHeader
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method formats the Row headers.
'
' @precon  None.
' @postcon Formats the Row headers.
'
Private Sub FormatRowHeader()
  Exception.Push "TRedAndGreen.FormatRowHeader"
  On Error GoTo ErrHnd
  ' Format Stagger
  Range(Sh.Cells(iRowDataStart, iColStagger), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColStagger)).NumberFormat = "[Blue]0 ""Right"";[Red]-0 ""Left"""
  ' Format Location
  Range(Sh.Cells(iRowDataStart, iColLocation), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColLocation)).NumberFormat = "#,##0.000"
  ' Conditional Format Area
  With Range(Sh.Cells(iRowDataStart, iColRouteNo), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColLocation))
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  ' Format Exclusions
  With Range(Sh.Cells(iRowDataStart, iColExclusions), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColExclusions))
    .Locked = False
    .HorizontalAlignment = xlHAlignCenter
    With .FormatConditions.Add(xlCellValue, xlEqual, "Yes")
      .Font.Color = 255
      .Interior.Color = 12632256
    End With
    .FormatConditions.Add(xlCellValue, xlEqual, "No").Font.Color = 16711680
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Section Exclusions"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether this Section should be excluded."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
  End With
  ' Auto Fit and Hide marker rows
  Range(Sh.Columns(iColRouteNo), Sh.Columns(iColExclusions)).Columns.AutoFit
  ' Border matrix area
  Range(Sh.Cells(iRowDataStart, iColExclusions), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColExclusions)).Borders(xlEdgeRight).Weight = xlMedium
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the data area of the matrix
'
' @precon  None.
' @postcon Outputs the data area of the matrix
'
Private Sub OutputDataArea()
  Dim iTask As Long
  Dim iSection As Long
  Exception.Push "TRedAndGreen.OutputDataArea"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting data area...", iProgress
  For iSection = 1 To DataMatrix.SectionCount
    For iTask = 1 To DataMatrix.TaskCount
      Sh.Cells(iSection + iRowDataStart, iTask + iColDataStart).Value = _
        DataMatrix.Time(iSection, iTask)
    Next iTask
  Next iSection
  FormatDataArea
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method formats the data area
'
' @precon  None.
' @postcon formats the data area
'
Private Sub FormatDataArea()
  Exception.Push "TRedAndGreen.FormatDataArea"
  On Error GoTo ErrHnd  ' Format numbers and conditional formats
  With Range(Sh.Cells(iRowDataStart, iColDataStart), Sh.Cells(iRowDataStart + _
    DataMatrix.SectionCount + 1, iColDataStart + DataMatrix.TaskCount + 1))
    .HorizontalAlignment = xlHAlignCenter
    .NumberFormat = "[h]:mm;[h]:mm;"
    .Font.Bold = True
    With .FormatConditions.Add(xlExpression, , _
      "=AND($" & GetColumnRef(iColStagger) & "$" & iRowTaskID & "=""Yes"",A1<0)")
      .Font.Color = 65535
      .Borders(xlBottom).Weight = xlHairline
      .Borders(xlLeft).Weight = xlHairline
      .Borders(xlRight).Weight = xlHairline
      .Borders(xlTop).Weight = xlHairline
      .Interior.Color = 255
    End With
    With .FormatConditions.Add(xlExpression, , _
      "=AND($" & GetColumnRef(iColStagger) & "$" & iRowTaskID & "=""Yes"",ISNUMBER(A1),A1>0)")
      .Font.Color = 16777215
      .Borders(xlBottom).Weight = xlHairline
      .Borders(xlLeft).Weight = xlHairline
      .Borders(xlRight).Weight = xlHairline
      .Borders(xlTop).Weight = xlHairline
      .Interior.Color = 32768
    End With
    .FormatConditions.Add(xlExpression, , "=OR(A$" & iRowTaskExc & "=""Yes"",$" & _
      GetColumnRef(iColExclusions) & iRowDataStart - iRowTaskExc & _
      "=""Yes"")").Interior.Color = 12632256
  End With
  Sh.Cells(iRowCatID, iColDescription).NumberFormat = """Contact Wire Date: ""ddd dd/mmm/yyyy;;"
  Sh.Cells(iRowCatID, iColDescription).Font.Bold = True
  Sh.Cells(iRowCatID, iColDescription).Font.Color = 128
  If DataMatrix.InstallDate = 0 Then
    Sh.Cells(iRowCatID, iColDescription) = Null
  Else
    Sh.Cells(iRowCatID, iColDescription) = DataMatrix.InstallDate
  End If
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the formulae for the column totals at the bottom of the matrix data area
'
' @precon  None.
' @postcon Outputs the formulae for the column totals at the bottom of the matrix data area
'
Private Sub OutputColTotals()
  Dim iTask As Long
  Dim strRange As String
  Dim strExclusions As String
  Dim iRow As Long
  Exception.Push "TRedAndGreen.OutputColTotals"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting column totals...", iProgress
  ' Titles
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColExclusions)
    .Value = "Total"
    .HorizontalAlignment = xlHAlignRight
    .Font.Bold = True
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 3, iColExclusions)
    .Value = "Completed"
    .HorizontalAlignment = xlHAlignRight
    .Font.Bold = True
    .Font.Color = 32768
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 4, iColExclusions)
    .Value = "Remaining"
    .HorizontalAlignment = xlHAlignRight
    .Font.Bold = True
    .Font.Color = 128
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 5, iColExclusions)
    .Value = "Work To Do"
    .HorizontalAlignment = xlHAlignRight
    .Font.Bold = True
    .Font.Color = 8388608
  End With
  ' Output Formulae
  iRow = iRowDataStart + DataMatrix.SectionCount + 1
  strExclusions = GetColumnRef(iColExclusions) & (iRowDataStart) & _
    ":" & GetColumnRef(iColExclusions) & iRow
  For iTask = 1 To DataMatrix.TaskCount
    strRange = GetColumnRef(iColDataStart + iTask) & (iRowDataStart) & _
      ":" & GetColumnRef(iColDataStart + iTask) & iRow
    ' Column Total Summation
    Sh.Cells(iRow + 1, iColDataStart + iTask).FormulaArray = _
      FormatString("=SUM((%param%>0)*(%param%))-SUM((%param%<0)*(%param%))", _
      strRange, strRange, strRange, strRange)
    ' Column Completed Summation
    Sh.Cells(iRow + 2, iColDataStart + iTask).FormulaArray = _
      FormatString("=SUM((%param%>0)*(%param%))", strRange, strRange)
    ' Column Remaining Summation
    Sh.Cells(iRow + 3, iColDataStart + iTask).FormulaArray = _
      FormatString("=ABS(SUM((%param%<0)*(%param%)))", strRange, strRange)
    ' Column Total Summation
    Sh.Cells(iRow + 4, iColDataStart + iTask).FormulaArray = _
      FormatString("=IF(%param%=""No"",ABS(SUM((%param%=""No"")*(%param%<0)*(%param%))), 0)", _
        GetColumnRef(iColDataStart + iTask) & iRowTaskExc, strExclusions, strRange, strRange)
  Next iTask
  FormatColTotals
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method formats the data area column totals.
'
' @precon  None.
' @postcon Formats the data area column totals.
'
Private Sub FormatColTotals()
  Exception.Push "TRedAndGreen.FormatColTotals"
  On Error GoTo ErrHnd
  ' Format Area
  With Range(Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 5, _
      iColDataStart + DataMatrix.TaskCount + 1))
    .Font.Bold = True
    .NumberFormat = "[h]:mm;[h]:mm;"
    .HorizontalAlignment = xlHAlignCenter
    .Rows(2).Font.Color = 32768
    .Rows(3).Font.Color = 128
    .Rows(4).Font.Color = 8388608
    .Borders(xlEdgeTop).Weight = xlMedium
  End With
  With Range(Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 5, _
      iColDataStart + DataMatrix.TaskCount + 1))
    .Columns.ColumnWidth = dblColWidth
    .FormatConditions.Add(xlExpression, , "=A$" & iRowTaskExc & "=""Yes""").Interior.Color = 12632256
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the formulae for the row totals at the right of the matrix data area
'
' @precon  None.
' @postcon Outputs the formulae for the row totals at the right of the matrix data area
'
Private Sub OutputRowTotals()
  Dim iStruc As Long
  Dim strRange As String
  Dim strExclusions As String
  Dim strMaterials As String
  Dim iCol As Long
  Exception.Push "TRedAndGreen.OutputRowTotals"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting row totals...", iProgress
  ' Titles
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 2) = "Total"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 3) = "Completed"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 3).Font.Color = 32768
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 4) = "Remaining"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 4).Font.Color = 128
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 5) = "Work To Do (Materials)"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 5).Font.Color = 8388608
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 6) = "Work To Do (Other)"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 6).Font.Color = 8388608
  With Range(Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 2), _
    Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + 6))
    .Font.Bold = True
    .HorizontalAlignment = xlHAlignCenter
    .Orientation = xlUpward
  End With
  ' Output Formulae
  iCol = iColDataStart + DataMatrix.TaskCount + 1
  strExclusions = GetColumnRef(iColDataStart) & iRowTaskExc & ":" & _
    GetColumnRef(iCol) & iRowTaskExc
  strMaterials = GetColumnRef(iColDataStart) & iRowTaskMats & ":" & _
    GetColumnRef(iCol) & iRowTaskMats
  For iStruc = 1 To DataMatrix.SectionCount
    strRange = GetColumnRef(iColDataStart) & iRowDataStart + iStruc & ":" & _
      GetColumnRef(iCol) & iRowDataStart + iStruc
    Sh.Cells(iRowDataStart + iStruc, iCol + 1).FormulaArray = _
      FormatString("=SUM((%param%>0)*(%param%))-SUM((%param%<0)*(%param%))", _
       strRange, strRange, strRange, strRange)
    Sh.Cells(iRowDataStart + iStruc, iCol + 2).FormulaArray = _
      FormatString("=SUM((%param%>0)*(%param%))", strRange, strRange)
    Sh.Cells(iRowDataStart + iStruc, iCol + 3).FormulaArray = _
      FormatString("=SUM((%param%<0)*(%param%))", strRange, strRange)
    Sh.Cells(iRowDataStart + iStruc, iCol + 4).FormulaArray = _
      FormatString("=IF(%param%=""No""," & _
        "ABS(SUM((%param%=""Yes"")*(%param%=""No"")*(%param%<0)*(%param%))),0)", _
        GetColumnRef(iColExclusions) & iRowDataStart + iStruc, _
        strMaterials, strExclusions, strRange, strRange)
    Sh.Cells(iRowDataStart + iStruc, iCol + 5).FormulaArray = _
      FormatString("=IF(%param%=""No""," & _
        "ABS(SUM((%param%=""No"")*(%param%=""No"")*(%param%<0)*(%param%))),0)", _
        GetColumnRef(iColExclusions) & iRowDataStart + iStruc, _
        strMaterials, strExclusions, strRange, strRange)
  Next iStruc
  FormatRowTotals
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method formats the data area row totals
'
' @precon  None.
' @postcon Formats the data area row totals
'
Private Sub FormatRowTotals()
  Exception.Push "TRedAndGreen.FormatRowTotals"
  On Error GoTo ErrHnd
  ' Format Area
  With Range(Sh.Cells(iRowDataStart, iColDataStart + DataMatrix.TaskCount + 2), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, _
    iColDataStart + DataMatrix.TaskCount + 6))
    .Font.Bold = True
    .NumberFormat = "[h]:mm;[h]:mm;"
    .HorizontalAlignment = xlHAlignCenter
    .Columns(2).Font.Color = 32768
    .Columns(3).Font.Color = 128
    .Columns(4).Font.Color = 8388608
    .Columns(5).Font.Color = 8388608
    .Borders(xlEdgeLeft).Weight = xlMedium
    .ColumnWidth = dblColWidth
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub


'
' This method outputs the over all totals for the data areas rows and columns
'
' @precon  None.
' @postcon Outputs the over all totals for the data areas rows and columns
'
Private Sub OutputOverAllTotals()
  Exception.Push "TRedAndGreen.OutputOverAllTotals"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting row totals...", iProgress
  ' Output Formulae
  Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 2) = _
    "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 3)
    .Value = "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
    .Font.Color = 32768
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 4)
    .Value = "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
    .Font.Color = 128
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 5)
    .Value = "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
    .Font.Color = 8388608
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 6)
    .Value = "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
    .Font.Color = 8388608
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 3, iColDataStart + DataMatrix.TaskCount + 2)
    .Value = "=SUM(RC[-" & DataMatrix.TaskCount + 2 & "]:RC[-1])"
    .Font.Color = 32768
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 4, iColDataStart + DataMatrix.TaskCount + 2)
    .Value = "=SUM(RC[-" & DataMatrix.TaskCount + 2 & "]:RC[-1])"
    .Font.Color = 128
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 5, iColDataStart + DataMatrix.TaskCount + 2)
    .Value = "=SUM(RC[-" & DataMatrix.TaskCount + 2 & "]:RC[-1])"
    .Font.Color = 8388608
  End With
  ' Format Area
  With Range(Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iColDataStart + DataMatrix.TaskCount + 2), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 5, iColDataStart + DataMatrix.TaskCount + 6))
    .HorizontalAlignment = xlHAlignCenter
    .NumberFormat = "[h]:mm;[h]:mm;"
    .Font.Bold = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the logic gates for letting work through
'
' @precon  None
' @postcon Outputs the logic gates for letting work through
'
Private Sub OutputGates()
  Exception.Push "TRedAndGreen.OutputGates"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting validation...", iProgress
  iGates = iColDataStart + DataMatrix.TaskCount + 7
  ' Titles
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset) = "Can work be done WRT the SWL"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset).Font.Color = 3355443
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1) = "Materials from Penkridge"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1).Font.Color = 8388736
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2) = "Materials from LDC"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2).Font.Color = 8388736
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3) = "Outstanding Technical Queries"
  Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3).Font.Color = 13056
  With Range(Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset), _
    Sh.Cells(iRowTaskDesc, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3))
    .Font.Bold = True
    .HorizontalAlignment = xlHAlignCenter
    .Orientation = xlUpward
    .ColumnWidth = dblColWidth
  End With
  ' Gates
  OutputSWLValidation
  OutputPenkMatsValidation
  OutputLDCMatsValidation
  OutputTQValidation
  Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColGatesOffset - 1).ColumnWidth = 2
  ' Group Gate columns
  Range(Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColGatesOffset), _
    Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3)).Columns.Group
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the safe working limit validation and totals
'
' @precon  None.
' @postcon Outputs the safe working limit validation and totals
'
Private Sub OutputSWLValidation()
  Dim strWorkToDo As String
  Dim strSWL As String
  Exception.Push "TRedAndGreen.OutputSWLValidation"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting SWL validation...", iProgress
  With Range(Sh.Cells(iRowDataStart, iColDataStart + DataMatrix.TaskCount + iColGatesOffset), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iColDataStart + DataMatrix.TaskCount + iColGatesOffset))
    .Font.Color = 3355443
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Safe Working Limit"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether this Section is " & _
        "affected by the Safe Working Limit."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
    .Value = "No"
    .Locked = False
    .HorizontalAlignment = xlHAlignCenter
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, _
    iColDataStart + DataMatrix.TaskCount + iColGatesOffset)
    strWorkToDo = GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + 6) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    strSWL = GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    .FormulaArray = FormatString("=SUM((%param%=""Yes"")*(%param%))", strSWL, strWorkToDo)
    .HorizontalAlignment = xlHAlignCenter
    .Font.Bold = True
    .Font.Color = 3355443
    .NumberFormat = "[h]:mm;[h]:mm;"
    If Reg.HideColumns Then .Columns.Hidden = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the Penkridge materials validation and totals
'
' @precon  None.
' @postcon Outputs the Penkridge materials validation and totals
'
Private Sub OutputPenkMatsValidation()
  Dim strWorkToDo As String
  Dim strPenkMats As String
  Exception.Push "TRedAndGreen.OutputPenkMatsValidation"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting Penkridge materials validation...", iProgress
  With Range(Sh.Cells(iRowDataStart, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1))
    .Font.Color = 8388736
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No,N/A"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Penkridge Materials"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes, No or N/A as to whether materials are available."
      .ErrorMessage = "This is not a valid input, please select either Yes, No or N/A."
      .ShowInput = True
      .ShowError = True
    End With
    .Value = "No"
    .Locked = False
    .HorizontalAlignment = xlHAlignCenter
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, _
    iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1)
    strWorkToDo = GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    strPenkMats = GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    .FormulaArray = FormatString("=SUM((%param%=""Yes"")*(%param%))", strPenkMats, strWorkToDo)
    .HorizontalAlignment = xlHAlignCenter
    .Font.Bold = True
    .Font.Color = 3355443
    .NumberFormat = "[h]:mm;[h]:mm;"
    If Reg.HideColumns Then .Columns.Hidden = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the LDC materials validation and totals
'
' @precon  None.
' @postcon Outputs the LDC materials validation and totals
'
Private Sub OutputLDCMatsValidation()
  Dim strWorkToDo As String
  Dim strLDCMats As String
  Exception.Push "TRedAndGreen.OutputLDCMatsValidation"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting LDC materials validation...", iProgress
  With Range(Sh.Cells(iRowDataStart, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2))
    .Font.Color = 8388736
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No,N/A"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "LDC Materials"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes, No or N/A as to whether materials are available."
      .ErrorMessage = "This is not a valid input, please select either Yes, No or N/A."
      .ShowInput = True
      .ShowError = True
    End With
    .Value = "No"
    .Locked = False
    .HorizontalAlignment = xlHAlignCenter
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, _
    iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2)
    strWorkToDo = GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    strLDCMats = GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    .FormulaArray = FormatString("=SUM((%param%=""Yes"")*(%param%))", strLDCMats, strWorkToDo)
    .HorizontalAlignment = xlHAlignCenter
    .Font.Bold = True
    .Font.Color = 3355443
    .NumberFormat = "[h]:mm;[h]:mm;"
    If Reg.HideColumns Then .Columns.Hidden = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the technical queries validation and totals
'
' @precon  None.
' @postcon Outputs the technical queries validation and totals
'
Private Sub OutputTQValidation()
  Dim strWorkToDo As String
  Dim strTQs As String
  Exception.Push "TRedAndGreen.OutputTQValidation"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting TQ validation...", iProgress
  With Range(Sh.Cells(iRowDataStart, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3))
    .Font.Color = 13056
    With .Validation
      .Delete
      .Add xlValidateList, xlValidAlertStop, xlBetween, "Yes,No"
      .IgnoreBlank = False
      .InCellDropdown = True
      .InputTitle = "Technical Queries"
      .ErrorTitle = .InputTitle
      .InputMessage = "Please enter Yes or No as to whether there are any Technical Queries Outstanding."
      .ErrorMessage = "This is not a valid input, please select either Yes or No."
      .ShowInput = True
      .ShowError = True
    End With
    .Value = "No"
    .Locked = False
    .HorizontalAlignment = xlHAlignCenter
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, _
    iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3)
    strWorkToDo = GetColumnRef(iColDataStart + DataMatrix.TaskCount + 5) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + 6) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    strTQs = GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3) & _
      (iRowDataStart) & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3) & _
      iRowDataStart + DataMatrix.SectionCount + 1
    .FormulaArray = FormatString("=SUM((%param%=""Yes"")*(%param%))", strTQs, strWorkToDo)
    .HorizontalAlignment = xlHAlignCenter
    .Font.Bold = True
    .Font.Color = 3355443
    .NumberFormat = "[h]:mm;[h]:mm;"
    If Reg.HideColumns Then .Columns.Hidden = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the summary information for the cat ids specified.
'
' @precon  None.
' @postcon Outputs the summary information for the cat ids specified.
'
Private Sub OutputCatIDColumns()
  Exception.Push "TRedAndGreen.OutputCatIDColumns"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting Cat ID columns...", iProgress
  CreateMaterialsNonMaterialsColumns iColDataStart + DataMatrix.TaskCount + iColMaterialsOffset
  Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColMaterialsOffset - 1).ColumnWidth = 2
  ' Group Cat Id columns
  With Range(Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColMaterialsOffset), _
    Sh.Columns(iColDataStart + DataMatrix.TaskCount + iColMaterialsOffset + 1))
    .Columns.Group
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method creates the category columns
'
' @precon  None.
' @postcon Creates the category columns
'
' @param   iStartCol as a Long
'
Private Sub CreateMaterialsNonMaterialsColumns(iStartCol As Long)
  Dim i As Long
  Dim iSt As Long
  Dim iEd As Long
  Dim strRange As String
  Dim strExclusions As String
  Dim strMaterials As String
  Dim strCatIDs As String
  Exception.Push "TRedAndGreen.CreateMaterialsNonMaterialsColumns", iStartCol
  On Error GoTo ErrHnd
  ' Titles
  Sh.Cells(iRowTaskDesc, iStartCol) = "Total Work Time (Materials)"
  Sh.Cells(iRowTaskDesc, iStartCol + 1) = "Total Work Time (Other)"
  With Range(Sh.Cells(iRowTaskDesc, iStartCol), Sh.Cells(iRowTaskDesc, iStartCol + 1))
    .Font.Bold = True
    .Font.Color = 16737843
    .Orientation = xlUpward
    .HorizontalAlignment = xlHAlignCenter
    .ColumnWidth = dblColWidth
  End With
  ' Formulae
  With Range(Sh.Cells(iRowDataStart, iStartCol), Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iStartCol + 1))
    .Font.Color = 16737843
    .HorizontalAlignment = xlHAlignCenter
    .NumberFormat = "[h]:mm;[h]:mm;"
    .FormatConditions.Add(xlExpression, , "=$" & GetColumnRef(iColExclusions) & "1=""Yes""").Interior.Color = 12632256
  End With
  iSt = iStartCol - iColDataStart + 2
  iEd = iStartCol - (iColDataStart + DataMatrix.TaskCount - 1)
  For i = iRowDataStart To iRowDataStart + DataMatrix.SectionCount + 1
    strRange = GetColumnRef(iColDataStart) & i & ":" & _
      GetColumnRef(iColDataStart + DataMatrix.TaskCount + 1) & i
    strMaterials = "$" & GetColumnRef(iColDataStart) & "$" & iRowTaskMats & ":" & _
      "$" & GetColumnRef(iColDataStart + DataMatrix.TaskCount + 1) & "$" & iRowTaskMats
    strExclusions = "$" & GetColumnRef(iColDataStart) & "$" & iRowTaskExc & ":" & _
      "$" & GetColumnRef(iColDataStart + DataMatrix.TaskCount + 1) & "$" & iRowTaskExc
    strCatIDs = "$" & GetColumnRef(iColDataStart) & "$" & iRowCatID & ":" & _
      "$" & GetColumnRef(iColDataStart + DataMatrix.TaskCount + 1) & "$" & iRowCatID
    Sh.Cells(i, iStartCol).FormulaArray = _
      FormatString("=IF(AND(%param%=""No"",%param%=""Yes""," & _
        "OR(%param%=""Yes"",%param%=""N/A"")," & _
        "OR(%param%=""Yes"",%param%=""N/A""),%param%=""No"")," & _
        "ABS(SUM((%param%=""Yes"")*(%param%=""No"")*(%param%<0)*(%param%))),0)", _
        GetColumnRef(iColExclusions) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 1) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 2) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3) & i, _
        strMaterials, strExclusions, strRange, strRange)
    Sh.Cells(i, iStartCol + 1).FormulaArray = _
      FormatString("=IF(AND(%param%=""No"",%param%=""Yes"",%param%=""No"")," & _
        "ABS(SUM((%param%=""No"")*(%param%=""No"")*(%param%<0)*(%param%))),0)", _
        GetColumnRef(iColExclusions) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset) & i, _
        GetColumnRef(iColDataStart + DataMatrix.TaskCount + iColGatesOffset + 3) & i, _
        strMaterials, strExclusions, strRange, strRange)
  Next i
  ' Summaries
  With Range(Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iStartCol), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iStartCol + 1))
    .Font.Bold = True
    .Font.Color = 16737843
    .HorizontalAlignment = xlHAlignCenter
    .NumberFormat = "[h]:mm;[h]:mm;"
    .Value = "=SUM(R[-" & DataMatrix.SectionCount + 2 & "]C:R[-1]C)"
    If Reg.HideColumns Then .Columns.Hidden = True
  End With
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method outputs the comments column and the Total Work To Do
'
' @precon  None.
' @postcon Outputs the comments column and the Total Work To Do
'
Private Sub OutputComments()
  Dim iStartCol As Long
  Dim str As String
  Dim i As Long
  Dim j As Long
  Exception.Push "TRedAndGreen.OutputComments"
  On Error GoTo ErrHnd
  Progress.Progress "Outputting comments...", iProgress
  ' Book keeping
  iStartCol = iColDataStart + DataMatrix.TaskCount + iColCommentsOffset
  ' Lines affected
  Sh.Cells(iRowTaskDesc, iStartCol).ColumnWidth = 100
  Sh.Cells(iRowTaskDesc, iStartCol) = "Lines Affected:" & vbLf & WireData.LinesAffected
  Sh.Cells(iRowTaskDesc, iStartCol).Font.Bold = True
  Sh.Cells(iRowTaskDesc, iStartCol).Font.Size = 18
  Sh.Cells(iRowTaskDesc, iStartCol).HorizontalAlignment = xlHAlignCenter
  Sh.Cells(iRowTaskDesc, iStartCol).VerticalAlignment = xlVAlignCenter
  Sh.Cells(iRowTaskExc, iStartCol) = "Comments:"
  Sh.Cells(iRowTaskExc, iStartCol).Font.Bold = True
  ' Comment Area
  With Range(Sh.Cells(iRowDataStart, iStartCol), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iStartCol))
    .Interior.Color = 10092543
    .Borders(xlEdgeTop).Weight = xlHairline
    .Borders(xlEdgeLeft).Weight = xlHairline
    .Borders(xlEdgeRight).Weight = xlHairline
    .Borders(xlEdgeBottom).Weight = xlHairline
    .Borders(xlInsideHorizontal).Weight = xlHairline
    .Locked = False
  End With
  For i = 1 To DataMatrix.SectionCount
    str = ""
    If Reg.SectionComment Then str = str & DataMatrix.SectionComment(i)
    If Reg.TaskComment Then
      For j = 1 To DataMatrix.TaskCount
        If DataMatrix.TaskComment(i, j) <> "" Then
          If str <> "" Then str = str & vbLf
          str = str & DataMatrix.TaskID(j) & ": " & DataMatrix.TaskComment(i, j)
        End If
      Next j
    End If
    If str <> "" Then Sh.Cells(iRowDataStart + i, iStartCol) = str
  Next
  ' Vertically align data center
  With Range(Sh.Cells(iRowDataStart, iColRouteNo), _
    Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 1, iStartCol - 1))
    .VerticalAlignment = xlVAlignCenter
  End With
  ' Summary
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 2, iStartCol)
    .NumberFormat = """Total Work Available To Do = ""[h]:mm"
    .Font.Bold = True
    .Font.Color = 16777215
    .Interior.Color = 32768
    .Borders(xlEdgeTop).Weight = xlThin
    .Borders(xlEdgeLeft).Weight = xlThin
    .Borders(xlEdgeRight).Weight = xlThin
    .Borders(xlEdgeBottom).Weight = xlThin
    .Value = "=SUM(RC[-" & iStartCol - iGates - 5 & "]:RC[-2])"
  End With
  ' Check Cell
  With Sh.Cells(iRowDataStart + DataMatrix.SectionCount + 4, iStartCol)
    .Font.Bold = True
    .Locked = False
    .Interior.Color = 39423
    .Borders(xlEdgeTop).Weight = xlThin
    .Borders(xlEdgeLeft).Weight = xlThin
    .Borders(xlEdgeRight).Weight = xlThin
    .Borders(xlEdgeBottom).Weight = xlThin
  End With
  Sh.Columns(iStartCol - 1).ColumnWidth = 2
  Sh.Cells(iRowTaskDesc, iStartCol).ColumnWidth = 50
  ' Group Comments
  Sh.Columns(iStartCol).Columns.Group
  If Reg.HideComments Then Sh.Columns(iStartCol).Hidden = True
ErrHnd:
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err
  Exception.Pop
End Sub

'
' This method sets up the page for printing
'
' @precon  None.
' @postcon Sets up the page for printing
'
Private Sub PageSetup()
  Exception.Push "TRedAndGreen.PageSetup"
  On Error GoTo ErrHnd
  Progress.Progress "Setting up page...", iProgress
  With Sh.PageSetup
    If Reg.TickSheetHeader Then
      .LeftHeader = "&""Arial,Bold""&12&UOutputTick Sheet"
      .CenterHeader = "&""Arial,Bold""&12&UShift:" & vbCr & "Date:" & vbCr & "Name:"
    Else
      .LeftHeader = "&F"
      .RightHeader = "&A"
      .LeftFooter = "Print Date: &D @ &T"
      .CenterFooter = strSignature
      .RightFooter = "Page &P of &N"
    End If
    .PrintGridlines = True
    .HeaderMargin = Application.InchesToPoints(WireData.TopMargin / 25.4)
    .TopMargin = Application.InchesToPoints(WireData.TopHeaderMargin / 25.4)
    .LeftMargin = Application.InchesToPoints(WireData.LeftMargin / 25.4)
    .RightMargin = Application.InchesToPoints(WireData.RightMargin / 25.4)
    .FooterMargin = Application.InchesToPoints(WireData.BottomMargin / 25.4)
    .BottomMargin = Application.InchesToPoints(WireData.BottomFooterMargin / 25.4)
    If WireData.PaperSize = iA4 Then .PaperSize = xlPaperA4 Else .PaperSize = xlPaperA3
    If WireData.PaperOrientation = iLandscape Then .Orientation = xlLandscape Else .Orientation = xlPortrait
    .Zoom = False
    .FitToPagesTall = 1
    .FitToPagesWide = 1
  End With
ErrHnd:
  ' Do not output 1004 errors to the log file.
  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err, (Err.Number <> 1004)
  Exception.Pop
End Sub
