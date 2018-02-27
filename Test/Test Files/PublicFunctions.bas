Attribute VB_Name = "PublicFunctions"
'
' This module contains public functions for use within the Red and Green work sheets.
'
' @Version 1.0
' @Date    12 Aug 2003
' @Author  David Hoyle
'
Option Explicit
Option Compare Text

'
' This is a worksheet function to return the number of hours available for work based on
' the current exclusions and materials requirements. This is for the rows only.
'
' @precon  Materials, Exclusions, and Times must be a valid Excel ranges.
' @postcon Returns the Work to do for the ranges based on the value of MatsReqd
'
' @param   Materials as a Range
' @param   Exclusions as a Range
' @param   Times as a Range
' @param   MatsReqd as a Boolean
' @return  a Double
'
' @noexception
' @noerror
'
Public Function WorkToDoInRows3(Materials As Range, Exclusions As Range, Times As Range, _
  MatsReqd As Boolean) As Double
  Dim i As Long
  Dim j As Long
  WorkToDoInRows3 = 0
  i = Materials.Count
  For j = 1 To i
    If Exclusions(, j) = "No" Then _
      If ((Materials(, j) = "Yes") Xor Not MatsReqd) Then _
        If IsNumeric(Times(, j)) Then _
          If Times(, j) < 0 Then _
            WorkToDoInRows3 = WorkToDoInRows3 + Abs(Times(, j))
  Next j
End Function

'
' This is a worksheet function to return the number of hours available for work based on
' the current exclusions and materials requirements. This is for the rows only.
'
' @precon  Materials, Exclusions, Times, and CatIDs must be a valid Excel ranges.
' @postcon Returns the Work to do for the ranges based on the value of MatsReqd and
'          iCatID
'
' @param   Materials as a Range
' @param   Exclusions as a Range
' @param   Times as a Range
' @param   CatIds as a Range
' @param   MatsReqd as a Boolean
' @param   iCatId as a Long
' @return  a Double
'
' @noexception
' @noerror
'
Public Function WorkToDoInRowsByCatID3(Materials As Range, Exclusions As Range, Times As Range, _
  CatIDs As Range, MatsReqd As Boolean, Optional iCatID As Long = -1) As Double
  Dim i As Long
  Dim j As Long
  WorkToDoInRowsByCatID3 = 0
  i = Materials.Count
  For j = 1 To i
    If iCatID = -1 Or CatIDs(, j) = iCatID Then _
      If IsNumeric(Times(, j)) Then _
        If Times(, j) < 0 Then If Exclusions(, j) = "No" Then _
          If ((Materials(, j) = "Yes") Xor Not MatsReqd) Then _
            WorkToDoInRowsByCatID3 = WorkToDoInRowsByCatID3 + Abs(Times(, j))
  Next j
End Function

'
' This is a worksheet function to return the number of hours available for work based on
' the current exclusions. This is for the columns only.
'
' @precon  Exclusions and Times must be a vaild excel range.
' @postcon Returns the work to do in the columns.
'
' @param   Exclusions as a Range
' @param   Times as a Range
' @return  a Double
'
' @noexception
' @noerror
'
Public Function WorkToDoInColumns3(Exclusions As Range, Times As Range) As Double
  Dim i As Long
  Dim j As Long
  WorkToDoInColumns3 = 0
  i = Exclusions.Count
  For j = 1 To i
    If Exclusions(j) = "No" Then _
      If IsNumeric(Times(j)) Then _
        If Times(j) < 0 Then _
          WorkToDoInColumns3 = WorkToDoInColumns3 + Abs(Times(j))
  Next j
End Function

