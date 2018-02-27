{*******************************************************}
{                                                       }
{       Smile Complaint Management                      }
{                                                       }
{       Copyright (c) 2004,05 Welcome CCS               }
{                                                       }
{*******************************************************}
unit uClassMgr;

interface

uses SysUtils, Classes, uAttribute;

type

  EInvalidAttributePath = class(Exception)
  public
    constructor Create; overload;
  end;

  {*
    Simple declaration for the TBOClass.
  }
  TBOClass = TBusinessObject;

  {*
    Defines a path within the class model.

    path should always start from a class (by id or by tblname).

    eg:
      #1.Call_ID
      or Call.EmpFup.Emp_FirstNm
  }
  TAttributePath = class(TObject)
  private
    // Attr path parts.
    FParts : TStringList;

    // Methods
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetAttribute: TAttribute;
    function GetBaseClass: TBOClass;
    function GetCount: Integer;
    function GetPart(Index: integer): string;
    procedure SetPart(Index: integer; const Value: string);
    function GetText: String;
    function GetAttrText: String;

  public
    // Contructors / Destructors
    constructor Create; overload;
    constructor Create(AClass: TBusinessObject); overload;
    constructor Create(ASource : TAttributePath); overload;
    constructor Create(ASource : String); overload;
    destructor Destroy; override;

    // Methods
    procedure Add(APart: String);
    procedure RemoveTail(ACount : Integer);
    property Value: String read GetValue write SetValue;
    property Text: String read GetText;
    property AttrText: String read GetAttrText;

    // Properties
    property Attribute: TAttribute read GetAttribute;
    property BaseClass: TBusinessObject read GetBaseClass;
    property Count : Integer read GetCount;
    property Parts[Index: integer] : string read GetPart write SetPart; default;
  end;

  TBOClasses = class(TObject)
  private
    FClasses : TList;
    function GetClass(index: integer): TBOClass;
    function GetCount: Integer;
  public
    // Constrcuting destructing
    constructor Create;
    destructor Destroy; override;

    // Methods
    procedure Refresh;

    // Properties
    property Classes[index: integer] : TBOClass read GetClass; default;
    property Count : Integer read GetCount;
  end;

  TClassManager = class(TObject)
  private
    FBOClasses : TBOClasses;
  public
    constructor Create;
    destructor Destroy; override;
    property BOClasses : TBOClasses read FBOClasses;
  end;

var
  ClassMgr : TClassManager;

implementation

uses StrUtils, uBOMgr;

{ TAttributePath }

procedure TAttributePath.Add(APart: String);
begin
  APart := Trim(APart);
  if (APart = '') then exit;
  FParts.Add(APart);
end;

constructor TAttributePath.Create(AClass: TBusinessObject);
begin
  Self.Create;
  FParts.Add('#' + IntToStr(AClass.ClassID));
end;

constructor TAttributePath.Create(ASource: TAttributePath);
begin
  Self.Create;
  FParts.Assign(ASource.FParts);
end;

constructor TAttributePath.Create;
begin
  FParts := TStringList.Create;
  FParts.Delimiter := '.';
end;

constructor TAttributePath.Create(ASource: String);
begin
  Self.Create;
  FParts.DelimitedText := ASource;
end;

destructor TAttributePath.Destroy;
begin
  FreeAndNil(FParts);
  inherited;
end;

function TAttributePath.GetAttribute: TAttribute;
var
  BOClass : TBusinessObject;
  Attr : TAttribute;
  I : Integer;

begin
  Attr := nil;
  BOClass := Self.BaseClass;

  // Walk through the model.
  for I := 1 to FParts.Count-1 do
  begin
    Attr := BOClass.GetAttribute(FParts[I]);
    if Attr = nil then
      raise EInvalidAttributePath.Create;

    if Attr is TSingleBOAttribute then
    begin
      BOClass := TSingleBOAttribute(Attr).GetClass;
    end;
  end;

  Result := Attr;
end;

function TAttributePath.GetAttrText: String;
var
  I : Integer;
  BOClass : TBOClass;
  Attr : TAttribute;

begin
  Result := '';

  BOClass := Self.BaseClass;
  if BOClass = nil then
    exit;

  // Walk the attr parts
  for I := 1 to FParts.Count-1 do
  begin
    if Result <> '' then
      Result := Result + ' - ';

    Attr := BOClass.GetAttribute(FParts[I]);
    if Attr <> nil then
      Result := Result + Attr.Name
    else
      Result := Result + '[nul]';

    if (Attr is TAssociationAttribute) then
      BOClass := TAssociationAttribute(Attr).GetClass;
  end;

  Result := Result;
end;

function TAttributePath.GetBaseClass: TBOClass;
begin
  Result := nil;
  if (FParts.Count = 0) then exit;
  if FParts[0][1] = '#' then
    Result := BOMgr.BOList.GetByID(StrToInt(
      RightStr(FParts[0],Length(FParts[0])-1)))
  else
    Result := BOMgr.BOList.GetByTblName(FParts[0]);
end;

function TAttributePath.GetCount: Integer;
begin
  Result := FParts.Count;
end;

function TAttributePath.GetPart(Index: integer): string;
begin
  Result := FParts[Index];
end;

{
  Returns the neath text corresponding to the attribute;
}
function TAttributePath.GetText: String;
var
  BOClass : TBOClass;
begin
  BOClass := Self.BaseClass;
  if BOClass = nil then
    exit;
  Result := BOClass.Name;
  Result := Result + ' - ' + GetAttrText;
end;

function TAttributePath.GetValue: String;
begin
  Result := FParts.DelimitedText;
end;

procedure TAttributePath.RemoveTail(ACount: Integer);
begin
  while (ACount > 0) and (FParts.Count > 0) do
  begin
    FParts.Delete(FParts.Count-1);
    Dec(ACount);
  end;
end;

procedure TAttributePath.SetPart(Index: integer; const Value: string);
begin
  FParts[Index] := Value;
end;

procedure TAttributePath.SetValue(const Value: String);
begin
  FParts.DelimitedText := Value;
end;

{ EInvalidAttributePath }

constructor EInvalidAttributePath.Create;
begin
  Create('Attribute path doesn''t correspond with the model');
end;

{ TBOClasses }

constructor TBOClasses.Create;
begin
  FClasses := TList.Create;
end;

destructor TBOClasses.Destroy;
begin
  FreeAndNil(FClasses);
  inherited;
end;

function TBOClasses.GetClass(index: integer): TBOClass;
begin
  Result := FClasses[Index];
end;

function TBOClasses.GetCount: Integer;
begin
  Result := FClasses.Count;
end;

procedure TBOClasses.Refresh;
begin
  FClasses.Free;
  FClasses := TList.Create;
  FClasses.Assign(BOMgr.BOList.List);
end;

{ TClassManager }

constructor TClassManager.Create;
begin
  FBOClasses := TBOClasses.Create;
end;

destructor TClassManager.Destroy;
begin
  FreeAndNil(FBOClasses);
  inherited;
end;

initialization

  ClassMgr := TClassManager.Create;

finalization

  ClassMgr.Free;

end.
