unit demobootstrap;

{$mode objfpc}

interface

uses
  sysutils, web, js, webwidget, htmlwidgets, bootstrapwidgets,  widgetdemo;

Type

  { TSimpleToastWidgetDemo }

  TSimpleToastWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TToastManagerDemo }

  TToastManagerDemo = class(TDemoContainer)
  private
    procedure DoShowToast(Sender: TObject; Event: TJSEvent);
  Protected
    FLabelClosable:TLabelWidget;
    FLabelHeader:TLabelWidget;
    FLabelBody:TLabelWidget;
    FLabelContextual:TLabelWidget;
    FHeader : TTextInputWidget;
    FBody : TTextAreaWidget;
    FContextual : TSelectWidget;
    FClosable : TCheckboxInputWidget;
    FShowButton: TBootstrapButton;
  public
    Class Function Demohelp : String; override;
    Class Function Description : String; override;
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Function GetInspectorInstance: TObject; override;
    Procedure ShowDemo; override;
  end;


  { TBootstrapButtonDemo }

  TBootstrapButtonDemo = class(TDemoContainer)
  Private
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

implementation

{ TToastManagerDemo }

procedure TToastManagerDemo.DoShowToast(Sender: TObject; Event: TJSEvent);

Var
  T : TContextual;
begin
  if FContextual.SelectedIndex<>-1 then
    T:=TContextual(FContextual.SelectedIndex)
  else
    T:=cNone;
  TToastManager.Instance.ShowToast(FHeader.Value,FBody.Lines.text,T,FClosable.Checked);
end;

class function TToastManagerDemo.Demohelp: String;
begin
  Result:='Toast manager demo: click button to show a toast in the top-left corner';
end;

class function TToastManagerDemo.Description: String;
begin
  Result:='Toast manager demo';
end;

class function TToastManagerDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=Nil;
end;

function TToastManagerDemo.GetInspectorInstance: TObject;
begin
  Result:=TToastManager.Instance;
end;

procedure TToastManagerDemo.ShowDemo;

Var
  T : TContextual;
  R : TRowWidget;
  C : TColWidget;

  Procedure AddRow;
  begin
    R:=TRowWidget.Create(Self);
    R.Parent:=Self;
    C:=TColWidget.Create(Self);
    C.Parent:=R;
  end;

begin
  inherited ShowDemo;
  TToastManager.Instance.ParentID:='toastarea-stack';
  // Contextual
  AddRow;
  FLabelContextual:=TLabelWidget.Create(Self);
  FLabelContextual.Text:='Contextual class for message';
  FContextual:=TSelectWidget.Create(Self);
  For T in TContextual do
    FContextual.Items.Add(ContextualNames[t]);
  FContextual.SelectedIndex:=0;
  FLabelContextual.LabelFor:=FContextual;
  FLabelContextual.Parent:=C;
  FContextual.Parent:=C;
  FContextual.Classes:='form-control';
  // Header
  AddRow;
  FLabelHeader:=TLabelWidget.Create(Self);
  FLabelHeader.Text:='Toast header';
  FHeader:=TTextInputWidget.Create(Self);
  FHeader.Value:='The message title';
  FLabelHeader.LabelFor:=FHeader;
  FLabelHeader.Parent:=C;
  FHeader.Parent:=C;
  FHeader.Classes:='form-control';
  // Body
  AddRow;
  FLabelBody:=TLabelWidget.Create(Self);
  FLabelBody.Text:='Toast body';
  FBody:=TTextAreaWidget.Create(Self);
  FBody.Lines.Text:='A nice message to show';
  FLabelBody.LabelFor:=FBody;
  FLabelBody.Parent:=C;
  FBody.Parent:=C;
  FBody.Classes:='form-control';
  // Closable checkbox
  AddRow;
  FLabelClosable:=TLabelWidget.Create(Self);
  FLabelClosable.Text:='Allow to close toast';
  FClosable:=TCheckboxInputWidget.Create(Self);
  FClosable.Classes:='form-check-input';
  FLabelClosable.LabelFor:=FClosable;
  FLabelClosable.Parent:=C;
  FClosable.Parent:=C;
  // button
  AddRow;
  FShowButton:=TBootstrapButton.Create(Self);
  FShowButton.Text:='Show toast';
  FShowButton.Parent:=C;
  FShowButton.OnClick:=@DoShowToast;
  Refresh;
end;

{ TBootstrapButtonDemo }

class function TBootstrapButtonDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TBootstrapButton;
end;

procedure TBootstrapButtonDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
end;

{ TToastManagerDemo }

class function TSimpleToastWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TSimpleToastWidget;
end;

procedure TSimpleToastWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
end;

initialization
  TBootstrapButtonDemo.RegisterDemo;
  TSimpleToastWidgetDemo.RegisterDemo;
  TToastManagerDemo.RegisterDemo;
end.

