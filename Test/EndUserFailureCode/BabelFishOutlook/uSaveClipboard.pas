unit uSaveClipboard;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, RichEdit, Clipbrd, registry;

type

  TSaveClipboard = class(TObject)
  private
    FTxt, FRTF : string;
    FBitmap : TBitmap;
    FMetafile : TMetafile;
    CF_RTF : UINT;
    procedure ClearBuffer;
  public
    constructor Create;reintroduce;
    destructor Destroy;override;
    procedure Save;
    procedure Restore;
  end;

implementation

{ TSaveClipboard }

procedure TSaveClipboard.ClearBuffer;
begin
  FTxt:='';
  FRTF:='';
  FBitmap.Free;
  FBitmap:=nil;
  FMetafile.Free;
  FMetafile:=nil;
end;

constructor TSaveClipboard.Create;
begin
  inherited Create;
  ClearBuffer;
  CF_RTF := RegisterClipboardFormat(RichEdit.CF_RTF);
end;

destructor TSaveClipboard.Destroy;
begin
  ClearBuffer;
  inherited;
end;

procedure TSaveClipboard.Restore;
var Data:THandle;
    DataPtr: Pointer;
begin
  try
    Clipboard.Open;
    //text
    if Length(FTxt) > 0 then Clipboard.AsText:=FTxt;
    //RTF
    if Length(FRTF) > 0 then begin
      Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Length(FRTF)+1);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(FRTF[1], DataPtr^, Length(FRTF)+1);
          SetClipboardData(CF_RTF, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    end;
    //BMP
    if FBitmap <> nil then Clipboard.Assign(FBitmap);
    //Metafile
    if FMetafile <> nil then Clipboard.Assign(FMetafile);
  finally
    Clipboard.Close;
  end;
end;

procedure TSaveClipboard.Save;
var Data:THandle;
begin
  ClearBuffer;
  //text
  if Clipboard.HasFormat(CF_TEXT) then FTxt:=Clipboard.AsText;
  //RTF
  if Clipboard.HasFormat(CF_RTF) then begin
    Data := GetClipboardData(CF_RTF);
    if Data <> 0 then begin
      FRTF := PChar(GlobalLock(Data));
      GlobalUnlock(Data);
    end;
  end;
  //Bitmap
  if Clipboard.HasFormat(CF_BITMAP) then
    try
      FBitmap:=TBitmap.Create;
      FBitmap.Assign(Clipboard);
    except
      FBitmap.Free;
    end;
  //metafile
  if Clipboard.HasFormat(CF_METAFILEPICT) then
    try
      FMetafile:=TMetafile.Create;
      FMetafile.Assign(Clipboard);
    except
      FMetafile.Free;
    end;
end;

end.
