unit Main;

interface  //################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Viewport3D, FMX.Types3D,
  FMX.MaterialSources, FMX.Objects3D, FMX.Controls3D,
  LUX, LUX.FMX, LUX.D3, LUX.Brep.Face.TriFlip.D3,
  LUX.Brep.Cell.TetraFlip.D3, LUX.Brep.Cell.TetraFlip.D3.Delaunay, LUX.Brep.Cell.TetraFlip.D3.FMX;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Button1: TButton;
    Light1: TLight;
    Dummy1: TDummy;
    Dummy2: TDummy;
    Camera1: TCamera;
    Grid3D1: TGrid3D;
    Panel1: TPanel;
    LightMaterialSourceD: TLightMaterialSource;
    LightMaterialSourceV: TLightMaterialSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { private 宣言 }
    _MouseP :TPointF;
    _MouseS :TShiftState;
  public
    { public 宣言 }
    _FaceModel  :TTriFaceModel3D;
    _Delaunay3D :TDelaunay3D;
    _DelaEdges  :TDelaEdges;
    _VoroEdges  :TVoroEdges;
    ///// メソッド
    function IsNearShell( const P_:TSingle3D ) :Boolean;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

function TForm1.IsNearShell( const P_:TSingle3D ) :Boolean;
var
   I :Integer;
begin
     with _FaceModel do
     begin
          for I := 0 to ChildsN-1 do
          begin
               with TTriFace3D( Childs[ I ] ).CircumSphere do
               begin
                    if Distance( Center, P_ ) <= Radius then
                    begin
                         Result := True;

                         Exit;
                    end;
               end;
          end;
     end;

     Result := False;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _MouseS := [];

     _FaceModel := TTriFaceModel3D.Create;
     _FaceModel.LoadFromFile( '..\..\_DATA\Torus.obj' );

     _Delaunay3D := TDelaunay3D.Create;

     _DelaEdges := TDelaEdges.Create( Self );
     _VoroEdges := TVoroEdges.Create( Self );

     with _DelaEdges do
     begin
          Parent     := Viewport3D1;
          Material   := LightMaterialSourceD;
          TetraModel := TTetraModel3D( _Delaunay3D );
          EdgeRadius := 0.005;
     end;

     with _VoroEdges do
     begin
          Parent     := Viewport3D1;
          Material   := LightMaterialSourceV;
          TetraModel := TTetraModel3D( _Delaunay3D );
          EdgeRadius := 0.005;
          EdgeLength := 0.005;
     end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Delaunay3D.Free;

     _FaceModel.Free;
end;

//------------------------------------------------------------------------------

procedure TForm1.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _MouseS := Shift;
     _MouseP := TPointF.Create( X, Y );
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
   P :TPointF;
begin
     if ssLeft in _MouseS then
     begin
          P := TPointF.Create( X, Y );

          with Dummy1.RotationAngle do Y := Y + ( P.X - _MouseP.X );
          with Dummy2.RotationAngle do X := X - ( P.Y - _MouseP.Y );

          _MouseP := P;
     end;
end;

procedure TForm1.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     Viewport3D1MouseMove( Sender, Shift, X, Y );

     _MouseS := [];
end;

//------------------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
var
   N, I :Integer;
   C :TDelaCell;
begin
     _Delaunay3D.DeleteChilds;

     with _FaceModel.PoinModel do
     begin
          for I := 0 to ChildsN-1 do _Delaunay3D.AddPoin( Childs[ I ].Pos );
     end;

     with _Delaunay3D do
     begin
          N := 0;
          while N < 100 do
          begin
               C := Childs[ Random( ChildsN ) ];

               if C.Open < 0 then
               begin
                    with C.CircumSphere do
                    begin
                         if _FaceModel.IsInside( Center ) and ( Radius > 0.1 ) and not IsNearShell( Center ) then
                         begin
                              AddPoin3( TDelaPoin.Create( Center, PoinModel ), C );

                              N := 0;
                         end
                         else Inc( N );
                    end;
               end;
          end;

          for I := ChildsN-1 downto 0 do
          begin
               with Childs[ I ] do
               begin
                    if ( Open >= 0 ) or not _FaceModel.IsInside( Barycenter ) then Free;
               end;
          end;

          SaveToFile( 'Model.tetf' );
     end;

     _DelaEdges.MakeModel;
     _VoroEdges.MakeModel;

     _DelaEdges.Geometry.SaveToFileBinSTL( 'Dera.stl', 'DelaEdges' );
     _VoroEdges.Geometry.SaveToFileBinSTL( 'Voro.stl', 'VoroEdges' );

     Viewport3D1.Repaint;
end;

end. //######################################################################### ■
