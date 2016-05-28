unit Main;

interface  //################################################################### Å°

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Viewport3D, FMX.Types3D,
  FMX.MaterialSources, FMX.Objects3D, FMX.Controls3D,
  LUX, LUX.D3, LUX.Brep.Face.TriFlip.D3,
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
    { private êÈåæ }
    _MouseP :TPointF;
    _MouseS :TShiftState;
  public
    { public êÈåæ }
    _FaceModel  :TTriFaceModel3D;
    _Delaunay3D :TDelaunay3D;
    _DelaEdges  :TDelaEdges;
    _VoroEdges  :TVoroEdges;
  end;

var
  Form1: TForm1;

implementation //############################################################### Å°

{$R *.fmx}

uses System.Math;

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
          EdgeRadius := 0.002;
     end;

     with _VoroEdges do
     begin
          Parent     := Viewport3D1;
          Material   := LightMaterialSourceV;
          TetraModel := TTetraModel3D( _Delaunay3D );
          EdgeRadius := 0.002;
          EdgeLength := 0.002;
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

function InsideTorus( const P_:TSingle3D ) :Boolean;
begin
     Result := ( Pow2( Roo2( Pow2( P_.X ) + Pow2( P_.Y ) ) - 1 ) + Pow2( P_.Z ) < 0.25 );
end;

procedure TForm1.Button1Click(Sender: TObject);
var
   I :Integer;
   C :TDelaCell;
   P01, P02, P03,
   P12, P23, P31 :TSingle3D;
begin
     _Delaunay3D.DeleteChilds;

     with _FaceModel.PoinModel do
     begin
          for I := 0 to ChildsN-1 do _Delaunay3D.AddPoin( Childs[ I ].Pos );
     end;

     with _Delaunay3D do
     begin
          for I := 1 to 10000 do
          begin
               C := Childs[ Random( ChildsN ) ];

               with C do
               begin
                    with CircumSphere do
                    begin
                         if InsideTorus( Center ) and ( Radius > 0.1 )
                         then AddPoin3( TDelaPoin.Create( Center, PoinModel ), C );
                    end;
               end;
          end;

          for I := ChildsN-1 downto 0 do
          begin
               C := Childs[ I ];
               with C do
               begin
                    P01 := Ave( Poin[ 0 ].Pos, Poin[ 1 ].Pos );
                    P02 := Ave( Poin[ 0 ].Pos, Poin[ 2 ].Pos );
                    P03 := Ave( Poin[ 0 ].Pos, Poin[ 3 ].Pos );
                    P12 := Ave( Poin[ 1 ].Pos, Poin[ 2 ].Pos );
                    P23 := Ave( Poin[ 2 ].Pos, Poin[ 3 ].Pos );
                    P31 := Ave( Poin[ 3 ].Pos, Poin[ 1 ].Pos );

                    if not InsideTorus( P01 ) or
                       not InsideTorus( P02 ) or
                       not InsideTorus( P03 ) or
                       not InsideTorus( P12 ) or
                       not InsideTorus( P23 ) or
                       not InsideTorus( P31 ) or
                       not InsideTorus( Barycenter ) or
                       not InsideTorus( CircumCenter ) then Free;
               end;
          end;
     end;

     _DelaEdges.MakeModel;
     _VoroEdges.MakeModel;

     Viewport3D1.Repaint;
end;

end. //######################################################################### Å°
