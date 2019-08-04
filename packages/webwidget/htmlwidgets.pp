unit htmlwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, webwidget, js, web;

Type

  { TButtonWidget }

  TButtonWidget = Class(TWebWidget)
  private
    FText: String;
    procedure SetText(AValue: String);
  Protected
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Procedure Click;
    Function HTMLTag : String; override;
    Property Text : String Read FText Write SetText;
  end;

  { TViewPort }

  TViewPort = Class(TCustomWebWidget)
  Private
    Class var FInstance : TViewPort;
  Protected
    Class Function FixedParent : TJSHTMLElement; override;
    Class Function FixedElement : TJSHTMLElement; override;
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
  Public
    Constructor Create (aOwner: TComponent); override;
    Function HTMLTag : String; override;
    Class Function Instance : TViewPort;
    Property Element;
  end;

  { TWebPage }

  TWebPage = Class(TCustomWebWidget)
  private
  Protected
    Class Function DefaultParentElement: TJSHTMLElement; override;
    Class Function DefaultParent : TCustomWebWidget; override;
    Procedure DoUnRender(aParent : TJSHTMLElement) ; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Function HTMLTag : String; override;
    // Later on, allow IFrame;
  Published
    Property ParentID;
    Property ElementID;
    Property Classes;
    Property Styles;
    Property StyleRefresh;
    Property Visible;
    // Events
    Property BeforeRenderHTML;
    Property AfterRenderHTML;
    Property OnAbort;
    Property OnAnimationCancel;
    Property OnAnimationEnd;
    Property OnAnimationIteration;
    Property OnAnimationStart;
    Property OnAuxClick;
    Property OnBlur;
    Property OnCancel;
    Property OnCanPlay;
    Property OnCanPlayThrough;
    Property OnChange;
    Property OnClick;
    Property OnCompositionEnd;
    Property OnCompositionStart;
    Property OnCompositionUpdate;
    Property OnContextMenu;
    Property OnCopy;
    Property OnCut;
    Property OnCueChange;
    Property OnDblClick;
    Property OnDurationChange;
    Property OnEnded ;
    Property OnError ;
    Property OnFocus;
    Property OnFocusIn ;
    Property OnFocusOut ;
    Property OnGotPointerCapture;
    Property OnInput;
    Property OnInvalid;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoad;
    Property OnLoadedData;
    Property OnLoadedMetaData;
    Property OnLoadend;
    Property OnLoadStart;
    Property OnLostPointerCapture;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseOut;
    Property OnMouseUp;
    Property OnOverFlow;
    Property OnPaste;
    Property OnPause;
    Property OnPlay;
    Property OnPointerCancel;
    Property OnPointerDown;
    Property OnPointerEnter;
    Property OnPointerLeave;
    Property OnPointerMove;
    Property OnPointerOut;
    Property OnPointerOver;
    Property OnPointerUp;
    Property OnReset;
    Property OnResize;
    Property OnScroll;
    Property OnSelect;
    Property OnSubmit;
    Property OnTouchStart;
    Property OnTransitionCancel;
    Property OnTransitionEnd;
    Property OnTransitionRun;
    Property OnTransitionStart;
    Property OnWheel;
  end;

  { TCustomInputWidget }

  TCustomInputWidget = Class(TWebWidget)
  private
    FValue : String;
    FValueName : String;
    FText : String;
    FReadOnly : Boolean;
    FRequired : Boolean;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetText: String;
    function GetValue: String;
    function GetValueName: String;
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetText(AValue: String);
    procedure SetValue(AValue: String);
    function GetInputElement: TJSHTMLInputElement;
    procedure SetValueName(AValue: String);
  Protected
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property InputElement : TJSHTMLInputElement Read GetInputElement;
    // Text to show (checkbox etc). Enable in descendents as needed
    Property Text : String Read GetText Write SetText;
  Public
    function InputType : String; virtual; abstract;
    Function HTMLTag : String; override;
    // Value as string
    Property Value : String Read GetValue Write SetValue;
    // Value Name to use when submitting using form.
    Property ValueName : String Read GetValueName Write SetValueName;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TTextInputWidget }

  TInputTextType = (ittText,ittPassword,ittNumber,ittEmail,ittSearch,ittTelephone,ittURL,ittColor);
  TTextInputWidget = class(TCustomInputWidget)
  private
    FMaxLength : Integer;
    FMinLength : Integer;
    FTextType : TInputTextType;
    function GetAsNumber: NativeInt;
    function GetMaxLength: NativeInt;
    function GetMinLength: NativeInt;
    function GetTextType: TInputTextType;
    procedure SetAsNumber(AValue: NativeInt);
    procedure SetMaxLength(AValue: NativeInt);
    procedure SetMinLength(AValue: NativeInt);
    procedure SetTextType(AValue: TInputTextType);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property TextType : TInputTextType Read GetTextType Write SetTextType;
    property AsNumber : NativeInt Read GetAsNumber Write SetAsNumber;
    Property MaxLength : NativeInt Read GetMaxLength Write SetMaxLength;
    Property MinLength : NativeInt Read GetMinLength Write SetMinLength;
    // Todo: List support
  end;


  { TButtonInputWidget }
  TInputButtonType = (ibtSubmit,ibtReset,ibtImage);
  TInputButtonTypes = set of TInputButtonType;

  TButtonInputWidget = class(TCustomInputWidget)
  private
    FButtonType: TInputButtonType;
    FSrc: String;
    procedure SetButtonType(AValue: TInputButtonType);
    procedure SetSrc(AValue: String);
  Public
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    function InputType : String; override;
    Class Function AllowChildren : Boolean; override;
  Published
    Property ButtonType : TInputButtonType Read FButtonType Write SetButtonType;
    Property Value;
    Property ValueName;
    Property Src : String Read FSrc Write SetSrc;
  end;

  { TCheckableInputWidget }

  TCheckableInputWidget = class(TCustomInputWidget)
  private
    FChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Property Value;
    Property ValueName;
    Property Checked : Boolean Read GetChecked Write SetChecked;
    Property Text;
  end;

  { TRadioInputWidget }

  TRadioInputWidget = class(TCheckableInputWidget)
  private
  Public
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;

  { TCheckboxInputWidget }

  TCheckboxInputWidget = class(TCheckableInputWidget)
  private
  Public
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;


  { TDateInputWidget }

  TDateInputWidget = class(TCustomInputWidget)
  private
    FDate: TDateTime;
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  Public
    function InputType : String; override;
    Class Function AllowChildren : Boolean; override;
  Published
    Property ValueName;
    Property Date : TDateTime Read GetDate Write SetDate;
  end;

  { TFileInputWidget }
  TFileInfo = record
    Name : String;
    TimeStamp : TDateTime;
    FileType : String;
    Size : NativeInt;
  end;

  TFileInputWidget = class(TCustomInputWidget)
  private
    FMultiple: Boolean;
    function GetFileCount: Integer;
    function GetFileDate(aIndex : Integer): TDateTime;
    function GetFileInfo(aIndex : Integer): TFileInfo;
    function GetFileName(aIndex : Integer): String;
    function GetFileSize(aIndex : Integer): NativeInt;
    function GetFileType(aIndex : Integer): String;
    function GetMultiple: Boolean;
    procedure SetMultiple(AValue: Boolean);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
    Property FileCount : Integer read GetFileCount;
    Property Files[aIndex : Integer] : String Read GetFileName;
    Property FileSizes[aIndex : Integer] : NativeInt Read GetFileSize;
    Property FileTypes[aIndex : Integer] : String Read GetFileType;
    Property FileDates[aIndex : Integer] : TDateTime Read GetFileDate;
    Property FileInfos[aIndex : Integer] : TFileInfo Read GetFileInfo;
  Published
    Property ValueName;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
  end;

  { THiddenInputWidget }

  THiddenInputWidget = class(TCustomInputWidget)
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
  Published
    Property ValueName;
    Property Value;
  end;

  { TTextAreaWidget }

  TTextAreaWrap = (tawSoft,tawHard,tawOff);
  TTextAreaWidget = Class(TWebWidget)
  private
    FLines: TStrings;
    FIgnoreChanges : Boolean;
    FMaxLength: Cardinal;
    FValueName : String;
    FRows,
    FColumns : Cardinal;
    FWrap: TTextAreaWrap;
    FRequired,
    FReadOnly : Boolean;
    procedure ApplyWrap(aElement: TJSHTMLTextAreaElement);
    procedure DoLineChanges(Sender: TObject);
    function GetColumns: Cardinal;
    function GetLines: TStrings;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetRows: Cardinal;
    function GetText: String;
    function GetValueName: string;
    procedure SetColumns(AValue: Cardinal);
    procedure SetLines(AValue: TStrings);
    procedure SetMaxLength(AValue: Cardinal);
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetRows(AValue: Cardinal);
    procedure SetText(AValue: String);
    procedure SetValueName(AValue: string);
    Function GetTextArea : TJSHTMLTextAreaElement;
    procedure SetWrap(AValue: TTextAreaWrap);
  Protected
    procedure ApplyLines(aElement: TJSHTMLTextAreaElement);
    procedure LinesFromHTML(aHTML : String);
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property TextArea :TJSHTMLTextAreaElement Read GetTextArea;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function AllowChildren : Boolean; override;
    Function HTMLTag : String; override;
    Property InnerHTML : String Read GetText Write SetText;
  Published
    Property ValueName : string Read GetValueName Write SetValueName;
    Property Rows : Cardinal Read GetRows Write SetRows;
    Property Columns : Cardinal Read GetColumns Write SetColumns;
    Property Lines : TStrings Read GetLines Write SetLines;
    Property MaxLength : Cardinal Read FMaxLength Write SetMaxLength;
    Property Wrap : TTextAreaWrap Read FWrap Write SetWrap;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TImageWidget }

  TImageWidget = class(TWebWidget)
  private
    FHeight: Integer;
    FWidth: Integer;
    FSrc : String;
    function GetHeight: Integer;
    function GetImg: TJSHTMLImageElement;
    function GetSrc: String;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetSrc(AValue: String);
    procedure SetWidth(AValue: Integer);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property ImgElement : TJSHTMLImageElement Read GetImg;
  Public
    Function HTMLTag : String; override;
  Published
    Property Src : String Read GetSrc Write SetSrc;
    Property Width : Integer Read GetWidth Write SetWidth;
    Property Height : Integer Read GetHeight Write SetHeight;
  end;

  { TSelectWidget }
  TJSHTMLOptionElementArray = Array of TJSHTMLOptionElement;
  TSelectWidget = class(TWebWidget)
  private
    FItems : TStrings;
    FValues : TStrings;
    FOptions : TJSHTMLOptionElementArray;
    FSelectedIndex : Integer;
    FMultiple : Boolean;
    function GetMultiple: Boolean;
    function GetSelected(Index : Integer): Boolean;
    function GetSelectedIndex: Integer;
    function GetItems: TStrings;
    function GetSelect: TJSHTMLSelectElement;
    function GetSelectionCount: Integer;
    function GetSelectionItem(aIndex : Integer): String;
    function GetSelectionValue(aIndex : Integer): String;
    function GetValues: TStrings;
    procedure OptionsChanged(Sender: TObject);
    procedure SetMultiple(AValue: Boolean);
    procedure SetSelected(Index : Integer; AValue: Boolean);
    procedure SetSelectedIndex(AValue: Integer);
    procedure setItems(AValue: TStrings);
    procedure setValues(AValue: TStrings);
  Protected
    Procedure BuildOptions(aSelect : TJSHTMLSelectElement); virtual;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property SelectElement : TJSHTMLSelectElement Read GetSelect;
    Property Options : TJSHTMLOptionElementArray Read Foptions;
  Public
    Constructor Create(aOWner : TComponent); override;
    Destructor Destroy; override;
    Function HTMLTag : String; override;
    // Items that are selected
    Property Selected[Index : Integer] : Boolean Read GetSelected Write SetSelected;
    Property SelectionCount : Integer Read GetSelectionCount;
    Property SelectionValue[aIndex : Integer] : String Read GetSelectionValue;
    Property SelectionItem[aIndex : Integer] : String Read GetSelectionItem;
  Published
    Property Items : TStrings Read GetItems Write setItems;
    Property Values : TStrings Read GetValues Write setValues;
    property SelectedIndex : Integer Read GetSelectedIndex Write SetSelectedindex;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
  end;

  { TLabelWidget }

  TLabelWidget = Class(TWebWidget)
  private
    FLabelFor: TWebWidget;
    FText: String;
    function GetLabelEl: TJSHTMLLabelElement;
    function GetText: String;
    procedure SetLabelFor(AValue: TWebWidget);
    procedure SetText(AValue: String);
  Protected
    procedure ApplyLabelFor(aLabelElement: TJSHTMLLabelElement);
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property LabelElement : TJSHTMLLabelElement Read GetLabelEl;
  Public
    Function HTMLTag : String; override;
    Property Text : String Read GetText Write SetText;
    Property LabelFor : TWebWidget Read FLabelFor Write SetLabelFor;
  end;


Function ViewPort : TViewPort;

implementation

uses DateUtils;

resourcestring
  SErrInvalidIndex = 'Index %s not in valid range of [0..%d]';

Function ViewPort : TViewPort;

begin
  Result:=TViewPort.Instance;
end;

{ TLabelWidget }

procedure TLabelWidget.ApplyLabelFor(aLabelElement : TJSHTMLLabelElement);

begin
  if Assigned(FlabelFor) then
    begin
    FlabelFor.EnsureElement;
    aLabelElement.for_:=FlabelFor.ElementID;
    end
  else
    aLabelElement.for_:='';
end;

procedure TLabelWidget.SetLabelFor(AValue: TWebWidget);
begin
  if (FLabelFor=AValue) then Exit;
  if Assigned(FLabelFor) then
    FLabelFor.RemoveFreeNotification(Self);
  FLabelFor:=AValue;
  if Assigned(FLabelFor) then
    FLabelFor.FreeNotification(Self);
  If IsRendered then
    ApplyLabelFor(LabelElement);
end;

function TLabelWidget.GetText: String;
begin
  if IsElementDirty then
    FText:=Element.InnerText;
  Result:=FText;
end;

function TLabelWidget.GetLabelEl: TJSHTMLLabelElement;
begin
  Result:=TJSHTMLLabelElement(Element);
end;

procedure TLabelWidget.SetText(AValue: String);
begin
  If Text=aValue then exit;
  Ftext:=aValue;
  If IsRendered then
    Element.innerText:=aValue;
end;

procedure TLabelWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FLabelFor) then
    FLabelFor:=Nil;
end;

procedure TLabelWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
    if Old=Text then
      Text:=Old;
end;

procedure TLabelWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  lbl : TJSHTMLLabelElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  lbl.InnerText:=Text;
  ApplyLabelFor(Lbl);
end;


function TLabelWidget.HTMLTag: String;
begin
  Result:='label';
end;

{ TSelectWidget }

function TSelectWidget.GetSelectedIndex: Integer;
begin
  if IsRendered then
    FSelectedIndex:=SelectElement.selectedIndex;
  Result:=FSelectedIndex
end;

function TSelectWidget.GetMultiple: Boolean;

begin
  if IsElementDirty then
    FMultiple:=SelectElement.multiple;
  Result:=FMultiple;
end;

function TSelectWidget.GetSelected(Index : Integer): Boolean;
begin
  if (Index<0) or (Index>=Length(Foptions)) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[Index,Length(Foptions)-1]);
  Result:=FOptions[Index].Selected
end;

function TSelectWidget.GetItems: TStrings;
begin
  Result:=FItems;
end;

function TSelectWidget.GetSelect: TJSHTMLSelectElement;
begin
  Result:=TJSHTMLSelectElement(Element);
end;

function TSelectWidget.GetSelectionCount: Integer;
begin
  Result:=SelectElement.selectedOptions.length;
end;

function TSelectWidget.GetSelectionItem(aIndex : Integer): String;
begin
  if (aIndex<0) or (aindex>=GetSelectionCount) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[aIndex,GetSelectionCount-1]);
  Result:=TJSHTMLOptionElement(SelectElement.selectedOptions.item(aIndex)).innerText;
end;

function TSelectWidget.GetSelectionValue(aIndex : Integer): String;
begin
  if (aIndex<0) or (aindex>=GetSelectionCount) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[aIndex,GetSelectionCount-1]);
  Result:=TJSHTMLOptionElement(SelectElement.selectedOptions.item(aIndex)).value;
end;

function TSelectWidget.GetValues: TStrings;
begin
  Result:=FValues;
end;

procedure TSelectWidget.OptionsChanged(Sender: TObject);
begin
  if IsRendered then
    BuildOptions(SelectElement);
end;

procedure TSelectWidget.SetMultiple(AValue: Boolean);
begin
  If (AValue=Multiple) then exit;
  FMultiple:=aValue;
  If IsRendered then
    SelectElement.multiple:=FMultiple;
end;

procedure TSelectWidget.SetSelected(Index : Integer; AValue: Boolean);
begin
  if (Index<0) or (Index>=Length(Foptions)) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[Index,Length(Foptions)-1]);
  FOptions[Index].Selected:=True;
end;

procedure TSelectWidget.SetSelectedIndex(AValue: Integer);

begin
  if (SelectedIndex=aValue) then
    Exit;
  FSelectedIndex:=aValue;
  if IsRendered then
    SelectElement.SelectedIndex:=FSelectedIndex;
end;

procedure TSelectWidget.setItems(AValue: TStrings);
begin
  If (AValue=FItems) then exit;
  FItems.Assign(aValue);
end;


procedure TSelectWidget.setValues(AValue: TStrings);
begin
  If (AValue=FValues) then exit;
  FValues.Assign(aValue);
end;

procedure TSelectWidget.BuildOptions(aSelect: TJSHTMLSelectElement);

Var
  O : TJSHTMLOptionElement;
  Itms,Vals : TStrings;
  I,Idx : Integer;

begin
  // Clear
  SetLength(FOptions,0);
  aSelect.InnerHTML:='';
  // Rebuild
  Itms:=Fitems;
  Vals:=FValues;
  Idx:=FSelectedIndex;
  SetLength(Foptions,Itms.Count);
  For I:=0 to Itms.Count-1 do
    begin
    O:=TJSHTMLOptionElement(CreateElement('option',''));
    FOptions[I]:=O;
    O.innerText:=Itms[I];
    if I<Vals.Count then
      O.value:=Vals[i];
    if I=Idx then
      O.selected:=True;
    aSelect.AppendChild(O);
    end;
end;

procedure TSelectWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

Var
  el : TJSHTmlSelectElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  BuildOptions(el);
end;

constructor TSelectWidget.Create(aOWner: TComponent);
begin
  inherited Create(aOWner);
  FItems:=TStringList.Create;
  TStringList(FItems).OnChange:=@OptionsChanged;
  FValues:=TStringList.Create;
  TStringList(FValues).OnChange:=@OptionsChanged;
end;

destructor TSelectWidget.Destroy;
begin
  inherited Destroy;
end;

function TSelectWidget.HTMLTag: String;
begin
  Result:='select';
end;

{ TImageWidget }

function TImageWidget.GetHeight: Integer;
begin
  if IsElementDirty then
    FHeight:=ImgElement.Height;
  Result:=Fheight;
end;

function TImageWidget.GetImg: TJSHTMLImageElement;
begin
  Result:=TJSHTMLImageElement(Element);
end;

function TImageWidget.GetSrc: String;
begin
  if IsElementDirty then
    FSrc:=ImgElement.Src;
  Result:=FSrc;
end;

function TImageWidget.GetWidth: Integer;
begin
  if IsElementDirty then
    FWidth:=ImgElement.Width;
  Result:=FWidth;
end;

procedure TImageWidget.SetHeight(AValue: Integer);
begin
  if AValue=Height then exit;
  FHeight:=AValue;
  If isrendered then
    ImgElement.Height:=aValue;
end;

procedure TImageWidget.SetSrc(AValue: String);
begin
  if AValue=Src then exit;
  FSrc:=AValue;
  If isrendered then
    ImgElement.Src:=FSrc;
end;

procedure TImageWidget.SetWidth(AValue: Integer);
begin
  if AValue=Width then exit;
  FWidth:=AValue;
  If isrendered then
    ImgElement.Width:=aValue;
end;

procedure TImageWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  img : TJSHTMLImageElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  Img.Src:=FSrc;
  Img.Height:=FHeight;
  Img.Width:=FWidth;
end;

function TImageWidget.HTMLTag: String;
begin
  Result:='img';
end;

{ TTextAreaWidget }

procedure TTextAreaWidget.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
end;

procedure TTextAreaWidget.SetMaxLength(AValue: Cardinal);
begin
  if FMaxLength=AValue then Exit;
  FMaxLength:=AValue;
  if IsRendered then
    TextArea.maxLength:=aValue;
end;

procedure TTextAreaWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
  if IsRendered then
    TextArea.Readonly:=FReadOnly;
end;

procedure TTextAreaWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
  if IsRendered then
    TextArea.Required:=FRequired;
end;

function TTextAreaWidget.GetColumns: Cardinal;
begin
  if IsElementDirty then
    FColumns:=TextArea.Cols;
  Result:=FColumns;
end;

procedure TTextAreaWidget.DoLineChanges(Sender: TObject);
begin
  if isRendered and not FIgnoreChanges then
    ApplyLines(TextArea);
end;


function TTextAreaWidget.GetLines: TStrings;
begin
  // We may want to change this to something more efficient. Maybe handle onchange
  // Note that if yo
  if IsElementDirty  then
    begin
    FIgnoreChanges:=True;
    try
      LinesFromHTML(Element.InnerHTml);
    finally
      FIgnoreChanges:=False;
    end;
    end;
  Result:=FLines;
end;

function TTextAreaWidget.GetReadOnly: Boolean;
begin
  if IsElementDirty then
    FReadonly:=TextArea.readOnly;
  Result:=FReadonly;
end;

function TTextAreaWidget.GetRequired: Boolean;
begin
  if IsElementDirty then
    FRequired:=TextArea.Required;
  Result:=FRequired;
end;

function TTextAreaWidget.GetRows: Cardinal;
begin
  if IsElementDirty then
    FRows:=TextArea.Rows;
  Result:=FRows;
end;

function TTextAreaWidget.GetText: String;
begin
  if IsElementDirty then
    Result:=Element.InnerHTML
  else
    Result:=FLines.Text;
end;

function TTextAreaWidget.GetValueName: string;
begin
  if IsElementDirty then
    FValueName:=Element.Name;
  Result:=FValueName;
end;

procedure TTextAreaWidget.SetColumns(AValue: Cardinal);
begin
  if AValue=FColumns then exit;
  FColumns:=aValue;
  if isRendered then
    TextArea.cols:=aValue;
end;

procedure TTextAreaWidget.SetRows(AValue: Cardinal);
begin
  if AValue=FRows then exit;
  FRows:=aValue;
  if isRendered then
    TextArea.Rows:=aValue;
end;

procedure TTextAreaWidget.SetText(AValue: String);
begin
  if isRendered then
    element.InnerText:=aValue
  else
    LinesFromHTML(aValue);
end;

procedure TTextAreaWidget.SetValueName(AValue: string);
begin
  if aValue=FValueName then exit;
  FValueName:=aValue;
  if IsRendered then
    TextArea.Name:=aValue;
end;

procedure TTextAreaWidget.SetName(const NewName: TComponentName);

var
  Old : String;
begin
  Old:=Name;
  inherited SetName(NewName);
  if csDesigning in ComponentState then
    begin
    if (FLines.Count=0) then
      FLines.Add(Name)
    else if (FLines.Count=1) and (FLines[0]=Old) then
      FLines[0]:=Name;
    end;
end;

procedure TTextAreaWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  area : TJSHTMLTextAreaElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  if FMaxLength>0 then
    area.maxlength:=FMaxLength;
  if FColumns>0 then
    area.cols:=FColumns;
  if FRows>0 then
    area.Rows:=FRows;
  if FLines.Count>0 then
    ApplyLines(area);
  if FValueName<>'' then
    area.Name:=FValueName;
  area.Readonly:=FReadOnly;
  area.Required:=FRequired;
  ApplyWrap(area);
end;


constructor TTextAreaWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines:=TStringList.Create;
  TStringList(FLines).OnChange:=@DoLineChanges;
  FColumns:=50;
  FRows:=10;
end;

destructor TTextAreaWidget.Destroy;
begin
  FreeAndNil(Flines);
  inherited;
end;

class function TTextAreaWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TTextAreaWidget.GetTextArea: TJSHTMLTextAreaElement;
begin
  Result:=TJSHTMLTextAreaElement(Element);
end;

procedure TTextAreaWidget.ApplyWrap(aElement :TJSHTMLTextAreaElement);

Const
  Wraps : Array[TTextAreaWrap] of string = ('soft','hard','off');

begin
  aElement.wrap:=Wraps[FWrap];
end;

procedure TTextAreaWidget.ApplyLines(aElement: TJSHTMLTextAreaElement);
begin
  aElement.innerHTML:=FLines.Text;
end;

procedure TTextAreaWidget.LinesFromHTML(aHTML: String);
begin
  FLines.Text:= StringReplace(aHTML,'<br>',sLineBreak,[rfIgnoreCase,rfReplaceAll]);
end;



procedure TTextAreaWidget.SetWrap(AValue: TTextAreaWrap);

begin
  if FWrap=AValue then Exit;
  FWrap:=AValue;
  if IsRendered then
    ApplyWrap(TextArea)
end;

function TTextAreaWidget.HTMLTag: String;
begin
  result:='textarea';
end;

{ TCheckboxInputWidget }

function TCheckboxInputWidget.InputType: String;
begin
  Result:='checkbox';
end;

{ TRadioInputWidget }

function TRadioInputWidget.InputType: String;
begin
  Result:='radio';
end;

{ THiddenInputWidget }

class function THiddenInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function THiddenInputWidget.InputType: String;
begin
  Result:='hidden';
end;

{ TFileInputWidget }

procedure TFileInputWidget.SetMultiple(AValue: Boolean);
begin
  if FMultiple=AValue then Exit;
  FMultiple:=AValue;
  if Isrendered then
    InputElement.multiple:=FMultiple;
end;

function TFileInputWidget.GetMultiple: Boolean;
begin
  if IsElementDirty  then
    FMultiple:=InputElement.multiple;
  Result:=FMultiple;
end;


function TFileInputWidget.GetFileName(aIndex : Integer): String;
begin
  Result:=InputElement.files.Files[aIndex].name;
end;

function TFileInputWidget.GetFileSize(aIndex : Integer): NativeInt;
begin
  Result:=InputElement.files.Files[aIndex].Size;
end;

function TFileInputWidget.GetFileType(aIndex : Integer): String;
begin
  Result:=InputElement.files.Files[aIndex]._Type;
end;

function TFileInputWidget.GetFileCount: Integer;
begin
  Result:=InputElement.files.Length;
end;

function TFileInputWidget.GetFileDate(aIndex : Integer): TDateTime;
begin
  Result:=JSDateToDateTime(InputElement.files.Files[aIndex].lastModifiedDate);
end;

function TFileInputWidget.GetFileInfo(aIndex : Integer): TFileInfo;

Var
  f : TJSHTMLFile;

begin
  F:=InputElement.files.Files[aIndex];
  Result.Name:=F.name;
  Result.Size:=F.size;
  Result.FileType:=F._type;
  Result.TimeStamp:= JSDateToDateTime(F.lastModifiedDate);
end;


procedure TFileInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

Var
  Old : String;

begin
  Old:=FValue;
  FValue:='';
  try
    inherited ApplyWidgetSettings(aElement);
    TJSHTMLInputElement(aElement).multiple:=FMultiple;
  finally
    FValue:=Old;
  end;
end;

class function TFileInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TFileInputWidget.InputType: String;
begin
  Result:='file';
end;


{ TDateInputWidget }

function TDateInputWidget.GetDate: TDateTime;

var
  aDate : TDateTime;

begin
  if IsElementDirty then
    begin
    aDate:=ScanDateTime('yyyy-mm-dd',Value);
    if aDate<>0 then
      FDate:=aDate;
    end;
  Result:=FDate;
end;


procedure TDateInputWidget.SetDate(AValue: TDateTime);
begin
  FDate:=aValue;
  Value:=FormatDateTime('yyyy-mm-dd',FDate);
end;

function TDateInputWidget.InputType: String;
begin
  Result:='date';
end;

class function TDateInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

{ TCheckableInputWidget }


procedure TCheckableInputWidget.SetChecked(AValue: Boolean);
begin
  // Get actual value
  if Checked=AValue then Exit;
  if isRendered then
    InputElement.checked:=aValue;
  FChecked:=AValue;
end;

procedure TCheckableInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  TJSHTMLInputElement(aElement).Checked:=FChecked;
end;


function TCheckableInputWidget.GetChecked: Boolean;
begin
  if IsElementDirty then
    FChecked:=InputElement.Checked;
  Result:=FChecked;
end;


{ TButtonInputWidget }

procedure TButtonInputWidget.SetButtonType(AValue: TInputButtonType);
begin
  if FButtonType=AValue then Exit;
  FButtonType:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TButtonInputWidget.SetSrc(AValue: String);
begin
  if FSrc=AValue then Exit;
  FSrc:=AValue;
  if IsRendered and (ButtonType=ibtImage) then
    Element.setAttribute('src',FSrc);
end;

procedure TButtonInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  if ButtonType=ibtImage then
    aElement.setAttribute('src',FSrc);
end;

function TButtonInputWidget.InputType: String;

Const
  Types : Array[TInputButtonType] of string = ('submit','reset','image');

begin
  Result:=Types[FButtonType]
end;

class function TButtonInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

{ TTextInputWidget }

function TTextInputWidget.GetAsNumber: NativeInt;
begin
  Result:=StrToIntDef(Value,0);
end;

function TTextInputWidget.GetMaxLength: NativeInt;
begin
  if IsElementDirty then
    FMaxLength:=InputElement.maxLength;
  Result:=FMaxLength;
end;

function TTextInputWidget.GetMinLength: NativeInt;
begin
  if IsElementDirty then
    FMinLength:=InputElement.minLength;
  Result:=FMinLength;
end;

function TTextInputWidget.GetTextType: TInputTextType;
begin
  Result:=FTextType;
end;

procedure TTextInputWidget.SetAsNumber(AValue: NativeInt);
begin
  Value:=IntToStr(aValue);
end;


procedure TTextInputWidget.SetMaxLength(AValue: NativeInt);
begin
  if (aValue=FMaxLength) then exit;
  FMaxLength:=aValue;
  if IsRendered then
    InputElement.maxLength:=FMaxLength;
end;

procedure TTextInputWidget.SetMinLength(AValue: NativeInt);
begin
  if (aValue=FMinLength) then exit;
  FMinLength:=aValue;
  if IsRendered then
    InputElement.minLength:=FMinLength;
end;

procedure TTextInputWidget.SetTextType(AValue: TInputTextType);
begin
  if aValue=FTextType then exit;
  FTextType:=aValue;
  if IsRendered then
    Refresh;
end;

procedure TTextInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  inp : TJSHTMLInputElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  if FMaxLength<>0 then
    inp.maxLength:=FMaxLength;
  if FMinLength<>0 then
    inp.minLength:=FMinLength;
end;

class function TTextInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TTextInputWidget.InputType: String;

Const
  Types : Array[TInputTextType] of string =
     ('text','password','number','email','search','tel','url','color');

begin
  Result:=Types[FTextType];
end;

{ TWebPage }

constructor TWebPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Classes:='WebPage';
end;


class function TWebPage.DefaultParentElement: TJSHTMLElement;
begin
  Result:=TViewport.Instance.Element;
end;

class function TWebPage.DefaultParent: TCustomWebWidget;
begin
  Result:=TViewport.Instance;
end;

procedure TWebPage.DoUnRender(aParent: TJSHTMLElement);
begin
  inherited DoUnRender(aParent);
end;

function TWebPage.HTMLTag: String;
begin
  Result:='div';
end;


{ TViewPort }

function TViewPort.HTMLTag: String;
begin
  Result:='body';
end;

class function TViewPort.FixedParent: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.documentElement);
end;

class function TViewPort.FixedElement: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.Body);
end;

function TViewPort.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=FixedElement;
end;

constructor TViewPort.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  EnsureElement;
end;

class function TViewPort.Instance: TViewPort;
begin
  if Finstance=Nil then
    FInstance:=TViewPort.Create(Nil);
  Result:=FInstance;
end;

{ TButtonWidget }

{ TButtonWidget }

procedure TButtonWidget.SetText(AValue: String);

begin
  if FText=AValue then Exit;
  FText:=AValue;
  if IsRendered then
     Element.InnerText:=Ftext;
end;


procedure TButtonWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (FText=Old) and (csDesigning in ComponentState) then
     FText:=NewName;
end;

function TButtonWidget.HTMLTag: String;
begin
  Result:='button';
end;

procedure TButtonWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  Inherited;
  aElement.InnerText:=Text;
end;

procedure TButtonWidget.Click;

begin
  DispatchEvent('click');
end;


{ TCustomInputWidget }

function TCustomInputWidget.GetValue: String;

Var
  Inp : TJSHTMLInputElement;
begin
  Inp:=InputElement;
  If Assigned(Inp) then
    Result:=Inp.value
  else
    Result:=FValue
end;

function TCustomInputWidget.GetText: String;
Var
  Inp : TJSHTMLElement;

begin
  Inp:=Element;
  If Assigned(Inp) then
    Result:=Inp.InnerText
  else
    Result:=FText;
end;

function TCustomInputWidget.GetReadOnly: Boolean;
begin
  if IsElementDirty then
    FReadonly:=InputElement.readOnly;
  Result:=FReadonly;
end;

function TCustomInputWidget.GetRequired: Boolean;
begin
  if IsElementDirty then
    FRequired:=InputElement.Required;
  Result:=FRequired;
end;

function TCustomInputWidget.GetValueName: String;

Var
  Inp : TJSHTMLInputElement;

begin
  Inp:=InputElement;
  If Assigned(Inp) then
    Result:=Inp.Name
  else
    begin
    Result:=FValueName;
    if Result='' then
      Result:=Name;
    end;
end;

procedure TCustomInputWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
  if IsRendered then
    InputElement.Readonly:=FReadOnly;
end;

procedure TCustomInputWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
  if IsRendered then
    InputElement.Required:=FRequired;
end;

procedure TCustomInputWidget.SetText(AValue: String);
Var
  Inp : TJSHTMLElement;

begin
  if aValue=Text then exit;
  FText:=aValue;
  Inp:=Element;
  If Assigned(Inp) then
    Inp.innerText:=aValue;
end;

procedure TCustomInputWidget.SetValue(AValue: String);

Var
  Inp : TJSHTMLInputElement;

begin
  if aValue=Value then exit;
  FValue:=aValue;
  Inp:=InputElement;
  If Assigned(Inp) then
    Inp.value:=aValue;
end;

procedure TCustomInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  Inp : TJSHTMLInputElement absolute aElement;

begin
  Inherited;
  if (ExternalElement) and (FValue='') then
    FValue:=TJSHTMLInputElement(aElement).value
  else
    begin
    Inp._type:=InputType;
    Inp.name:=FValueName;
    Inp.value:=FValue;
    Inp.Required:=FRequired;
    Inp.ReadOnly:=FReadOnly;
    end;
end;

function TCustomInputWidget.HTMLTag: String;
begin
  Result:='input';
end;

function TCustomInputWidget.GetInputElement: TJSHTMLInputElement;
begin
  Result:=TJSHTMLInputElement(Element);
end;

procedure TCustomInputWidget.SetValueName(AValue: String);
Var
  Inp : TJSHTMLInputElement;
begin
  if aValue=ValueName then exit;
  FValueName:=aValue;
  Inp:=InputElement;
  If Assigned(Inp) then
    Inp.name:=aValue;
end;

procedure TCustomInputWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (Value=Old) then
    Value:=NewName;
end;

end.

