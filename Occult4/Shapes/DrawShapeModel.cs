using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult;

namespace Shapes
{
	internal class DrawShapeModel
	{
		public const double Radian = 180.0 / Math.PI;

		internal static List<Model_Parameters> SingleAst = new List<Model_Parameters>();

		private static List<Vector> Vertices = new List<Vector>();

		private static List<Vector> EquatorialVertices = new List<Vector>();

		private static List<Vector> PlotVertices = new List<Vector>();

		private static List<PlotCoords> PlotData = new List<PlotCoords>();

		private static List<Vector> FaceNormals = new List<Vector>();

		private static List<Face> Faces = new List<Face>();

		private static List<Vector> PlotAxes = new List<Vector>();

		private static List<AsteroidElements> ElementsList = new List<AsteroidElements>();

		private static List<AsteroidElements> ElementsAt1Day = new List<AsteroidElements>();

		private static Vector SunFromAsteroid = new Vector();

		private static Vector EarthFromAsteroid = new Vector();

		private static Vector Camera_To = new Vector();

		private static Vector Camera_Up = new Vector();

		private static Vector Camera_Right = new Vector();

		private static Matrix ToEquator = new Matrix();

		internal static double Lambda;

		internal static double Beta;

		internal static double Period;

		internal static double YORP;

		internal static double JD0;

		internal static double Phi0;

		internal static string Quality = "";

		internal static string Version = "";

		internal static string Comment = "";

		internal static string Created = "";

		internal static string Modified = "";

		internal static double Surf_Equiv_Radius_OnPlot = 1.0;

		internal static double Vol_Equiv_Radius_OnPlot = 1.0;

		internal static double Surface_Equiv_Radius_OfModel = 1.0;

		internal static double Volume_Equiv_Radius_OfModel = 1.0;

		internal static double MaxZ = 0.0;

		internal static double MinZ = 0.0;

		internal static string AstNumber = "";

		internal static string AsteroidName = "";

		internal static string ModelNumber = "";

		private static bool IsBinary = false;

		private static int BinaryBoundary = 0;

		internal static double PAofAxis = 0.0;

		internal static double AngleOfAxisAboveSkyPlane = 0.0;

		internal static float[] ImagePlotScale_1Radius = new float[2] { 0.49f, 0.45454544f };

		internal static bool GetDataToOrientTheModel(double JD, int AsteroidNumber)
		{
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Getting data to orient the model");
			((Control)messageForm).Show();
			Application.DoEvents();
			ToEquator = new Matrix();
			ToEquator.SetMatrixValues(1.0, 0.0, 0.0, 0.0, Utilities.cosEcliptic2000[Utilities.EclipticID], 0.0 - Utilities.sinEcliptic2000[Utilities.EclipticID], 0.0, Utilities.sinEcliptic2000[Utilities.EclipticID], Utilities.cosEcliptic2000[Utilities.EclipticID]);
			JPL_DE.Sun_FromEarth_ForHeliocentricOrbits(JD, 2.0, out var x, out var y, out var z, out var _);
			Vector vector = new Vector();
			vector.SetVectorValues(x, y, z);
			bool flag = false;
			bool flag2 = false;
			ElementsList = Elements.MainAsteroids.AstElements;
			if (ElementsList.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			int index = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(AsteroidNumber);
			Utilities.Date_from_JD(JD, out var Year, out var Month, out var day);
			if (day == 0.0)
			{
				day += 0.1;
			}
			int num = (int)Math.Floor(day);
			double epochHour = 24.0 * (day - (double)num);
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\HorizonForReductions.csv"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\HorizonForReductions.csv");
				streamReader.ReadLine();
				string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
				if (AsteroidNumber.ToString() == array[0])
				{
					double num2 = Utilities.JD_from_Date(array[2], array[3], array[4], "");
					if (Math.Abs(JD - num2) < 2.0)
					{
						flag2 = true;
						flag = true;
					}
				}
			}
			if (!flag2)
			{
				if (Utilities.InternetIsAvailable())
				{
					AsteroidElements AE = new AsteroidElements();
					string horizonsElements_TDBtime = http.GetHorizonsElements_TDBtime(AsteroidNumber.ToString(), Year, Month, num, epochHour, ref AE);
					if (horizonsElements_TDBtime.Length > 50)
					{
						using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\HorizonForReductions.csv"))
						{
							streamWriter.WriteLine(AsteroidElements.CSVHeader());
							streamWriter.WriteLine(horizonsElements_TDBtime);
							flag = true;
						}
						index = 0;
					}
				}
				if (!flag)
				{
					MessageBox.Show("Asteroid " + AsteroidNumber + " is not in the Occult file of asteroid orbital elements.\r\n\r\nAlso the orbital elements could not be downloaded from Horizons.\r\n\r\nTo plot this model you will need to either\r\n\r\n1. connect to the internet to download from Horizons, or\r\n\r\n2. recreate the file of asteroid elements by converting one of ASTORB, MPCORB or AstDys2 - either by using a smaller diameter, or by specifying this asteroid.", "Missing asteroid", (MessageBoxButtons)0, (MessageBoxIcon)16);
					((Form)messageForm).Close();
					return false;
				}
			}
			string diaSource = "";
			double EpochMeanAnomaly;
			double perihelion;
			double node;
			double i;
			double e;
			double a;
			double q;
			double h;
			double g_phaseCoeff;
			double logR_Coeff;
			double OsculatingDate;
			double MeanDiameter;
			double DiameterUncertainty;
			if (flag)
			{
				ElementsList = Elements.Reductions.AstElements;
				ElementsList.Clear();
				Elements.Reductions.Fill_AllAsteroids();
				AstNumber = "(" + Elements.Reductions.AstElements[0].IDNumber + ")";
				AsteroidName = Elements.Reductions.AstElements[0].IDName;
				EpochMeanAnomaly = Elements.Reductions.AstElements[0].Meananomaly / (180.0 / Math.PI);
				perihelion = Elements.Reductions.AstElements[0].Perihelion / (180.0 / Math.PI);
				node = Elements.Reductions.AstElements[0].Node / (180.0 / Math.PI);
				i = Elements.Reductions.AstElements[0].I / (180.0 / Math.PI);
				e = Elements.Reductions.AstElements[0].E;
				a = Elements.Reductions.AstElements[0].A;
				q = a * (1.0 - e);
				h = Elements.Reductions.AstElements[0].H0;
				g_phaseCoeff = Elements.Reductions.AstElements[0].G_phaseCoeff;
				logR_Coeff = Elements.Reductions.AstElements[0].LogR_Coeff;
				OsculatingDate = Elements.Reductions.AstElements[0].OsculatingJD;
				int iDNumber = Elements.Reductions.AstElements[0].IDNumber;
				Utilities.OpenAsteroidDiameterFileForReading();
				Utilities.GetAsteroidDiameter(iDNumber, h, out MeanDiameter, out DiameterUncertainty, out var _, out var _);
				Utilities.CloseAsteroidDiameterFileForReading();
			}
			else
			{
				AstNumber = "(" + ElementsList[index].IDNumber + ")";
				AsteroidName = ElementsList[index].IDName;
				OsculatingDate = ElementsList[index].OsculatingJD;
				EpochMeanAnomaly = ElementsList[index].Meananomaly / (180.0 / Math.PI);
				perihelion = ElementsList[index].Perihelion / (180.0 / Math.PI);
				node = ElementsList[index].Node / (180.0 / Math.PI);
				i = ElementsList[index].I / (180.0 / Math.PI);
				e = ElementsList[index].E;
				a = ElementsList[index].A;
				q = a * (1.0 - e);
				h = ElementsList[index].H0;
				g_phaseCoeff = ElementsList[index].G_phaseCoeff;
				logR_Coeff = ElementsList[index].LogR_Coeff;
				MeanDiameter = ElementsList[index].Diameter_Mean;
				diaSource = ElementsList[index].Dia_Source;
			}
			Utilities.NumericIntegrate(JD - 1.0, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref q, ref e, ref i, ref node, ref perihelion, saveflag: false, 0, AsteroidName, h, g_phaseCoeff, logR_Coeff, MeanDiameter, diaSource, 0.0, 0, 0, "", "", "", 0.0, 0.0, 0.0, ElementsAt1Day);
			double num3 = a * (1.0 - e);
			double TrueAnomaly;
			if (e == 0.0)
			{
				TrueAnomaly = EpochMeanAnomaly;
			}
			else if (e == 1.0)
			{
				double num4 = (JD - OsculatingDate) * Math.Sqrt((1.0 + e) / 2.0);
				Utilities.Kepler(0.01720209895 * num4 / Math.Pow(num3, 1.5), e, out TrueAnomaly, out DiameterUncertainty);
			}
			else
			{
				double num5 = 0.01720209895 * Math.Pow(Math.Abs(num3 / (1.0 - e)), -1.5);
				Utilities.Kepler(EpochMeanAnomaly + num5 * (JD - OsculatingDate), e, out TrueAnomaly, out DiameterUncertainty);
			}
			double num6 = q * (e + 1.0) / (1.0 + e * Math.Cos(TrueAnomaly));
			double num7 = TrueAnomaly + perihelion;
			double num8 = num6 * (Math.Cos(num7) * Math.Cos(node) - Math.Sin(num7) * Math.Sin(node) * Math.Cos(i));
			double num9 = num6 * (Math.Cos(num7) * Math.Sin(node) + Math.Sin(num7) * Math.Cos(node) * Math.Cos(i));
			double num10 = num6 * (Math.Sin(num7) * Math.Sin(i));
			SunFromAsteroid.SetVectorValues(0.0 - num8, 0.0 - num9, 0.0 - num10);
			SunFromAsteroid = MatrixOps.Rotation(ToEquator, SunFromAsteroid);
			EarthFromAsteroid = MatrixOps.Addition(SunFromAsteroid, vector);
			Camera_Up.SetVectorValues(0.0, 0.0, -1.0);
			Camera_To = MatrixOps.UnitVector(MatrixOps.Multiplication(EarthFromAsteroid, -1.0));
			Camera_Right = MatrixOps.UnitVector(MatrixOps.CrossProduct(Camera_To, Camera_Up));
			Camera_Up = MatrixOps.UnitVector(MatrixOps.CrossProduct(Camera_Right, Camera_To));
			((Form)messageForm).Close();
			return true;
		}

		internal static bool PopulateDataElementsForTheShapeModel(string ShapeModelFileName, int ISAMindex)
		{
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ce: Unknown result type (might be due to invalid IL or missing references)
			ModelNumber = ShapeModelFileName;
			Surf_Equiv_Radius_OnPlot = (Vol_Equiv_Radius_OnPlot = 0.0);
			double num = 0.0;
			Vertices.Clear();
			Faces.Clear();
			string text = GetShapeModelData.ShapeModelDirectory + ShapeModelFileName + ".txt";
			if (!File.Exists(text))
			{
				MessageBox.Show("The file containing the shape model data is not present on this computer", "No shape model file", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return false;
			}
			long length = new FileInfo(text).Length;
			if (length < 10000)
			{
				string text2 = GetShapeModelData.AsteroidNumberFromDamitID(Path.GetFileNameWithoutExtension(ShapeModelFileName));
				MessageBox.Show("The shape model file \r\n    " + text + "\r\nfor asteroid (" + text2 + ") has a length of  " + length + " bytes. \r\n\r\nValid shape model files have a file length\r\ngreater than 13,000 bytes\r\n\r\nNo shape model will be generated from this file.", "Bad shape model file", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return false;
			}
			using (StreamReader streamReader = new StreamReader(text))
			{
				string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
				int num2 = int.Parse(array[0]);
				int num3 = int.Parse(array[1]);
				int num4 = 0;
				for (int i = 0; i < num2; i++)
				{
					string[] array2 = streamReader.ReadLine()!.Split(new char[1] { ',' });
					Vector vector = new Vector();
					vector.X = double.Parse(array2[0]);
					vector.Y = double.Parse(array2[1]);
					vector.Z = double.Parse(array2[2]);
					if (i > 0 && Math.Sign(vector.X) != Math.Sign(Vertices[i - 1].X))
					{
						num4++;
						BinaryBoundary = 2 * i - 4;
					}
					Vertices.Add(vector);
				}
				streamReader.ReadLine();
				for (int j = 0; j < num3; j++)
				{
					string[] array3 = streamReader.ReadLine()!.Split(new char[1] { ',' });
					Face face = new Face();
					face.F1 = int.Parse(array3[0]);
					face.F2 = int.Parse(array3[1]);
					face.F3 = int.Parse(array3[2]);
					Faces.Add(face);
				}
				IsBinary = num4 == 1;
			}
			for (int k = 0; k < Vertices.Count; k++)
			{
				double num5 = Math.Abs(MatrixOps.Modulus(Vertices[k]));
				if (num5 > num)
				{
					num = num5;
				}
			}
			for (int l = 0; l < Vertices.Count; l++)
			{
				Vertices[l].X /= num;
				Vertices[l].Y /= num;
				Vertices[l].Z /= num;
			}
			double num6 = 0.0;
			double num7 = 0.0;
			for (int m = 0; m < Faces.Count; m++)
			{
				Vector vector2 = Vertices[Faces[m].F1];
				Vector v = Vertices[Faces[m].F2];
				Vector v2 = Vertices[Faces[m].F3];
				Vector v3 = MatrixOps.Subtraction(v, vector2);
				Vector v4 = MatrixOps.Subtraction(v2, vector2);
				Vector vector3 = MatrixOps.CrossProduct(v3, v4);
				double num8 = MatrixOps.Modulus(vector3) / 2.0;
				double value = MatrixOps.DotProduct(vector2, vector3) / 6.0;
				num6 += num8;
				num7 += Math.Abs(value);
			}
			Surface_Equiv_Radius_OfModel = Math.Sqrt(num6 / Math.PI) / 2.0;
			Volume_Equiv_Radius_OfModel = Math.Pow(3.0 * num7 / 4.0 / Math.PI, 1.0 / 3.0);
			double num9 = 1.0;
			double num10 = 1.0;
			double num11 = 1.0;
			double num12 = 1.0;
			double num13 = 1.0;
			double num14 = 1.0;
			double num15 = -1.0;
			double num16 = -1.0;
			MinZ = 1.0;
			MaxZ = -1.0;
			for (int n = 0; n < Vertices.Count; n++)
			{
				double num17 = Vertices[n].X * Vertices[n].X + Vertices[n].Y * Vertices[n].Y;
				if (Vertices[n].Z > 0.0)
				{
					if (num17 < num9)
					{
						num11 = num9;
						num14 = num13;
						num9 = num17;
						num13 = Vertices[n].Z;
					}
				}
				else if (num17 < num10)
				{
					num12 = num10;
					num16 = num15;
					num10 = num17;
					num15 = Vertices[n].Z;
				}
			}
			MaxZ = num13 + num9 / (num9 + num11) * (num14 - num13);
			MinZ = num15 + num10 / (num10 + num12) * (num16 - num15);
			if (int.TryParse(ShapeModelFileName, out var _))
			{
				for (int num18 = 0; num18 < GetShapeModelData.DAMITModelsByAsteroidNumber.Count; num18++)
				{
					if (ShapeModelFileName == GetShapeModelData.DAMITModelsByAsteroidNumber[num18].ModelNumber_String)
					{
						Lambda = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Lambda);
						Beta = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Beta);
						Period = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Period);
						YORP = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].YORP);
						JD0 = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].JD0);
						Phi0 = double.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Phi0);
						Quality = GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Quality;
						Version = GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Version;
						Comment = GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Comment;
						Created = GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Created;
						Modified = GetShapeModelData.DAMITModelsByAsteroidNumber[num18].Modified;
						GetShapeModelData.DAMITModelsByAsteroidNumber[num18].VolumeEquivalentRadius = Volume_Equiv_Radius_OfModel;
						GetShapeModelData.DAMITModelsByAsteroidNumber[num18].SurfaceEquivalentRadius = Surface_Equiv_Radius_OfModel;
						break;
					}
				}
			}
			else
			{
				Lambda = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Lambda);
				Beta = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Beta);
				Period = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Period);
				YORP = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].YORP);
				JD0 = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].JD0);
				Phi0 = double.Parse(GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Phi0);
				Quality = GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Quality;
				Version = GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Version;
				Comment = GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Comment;
				Created = GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Created;
				Modified = GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].Modified;
				GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].VolumeEquivalentRadius = Volume_Equiv_Radius_OfModel;
				GetShapeModelData.ISAMmodelsByAsteroidNumber[ISAMindex].SurfaceEquivalentRadius = Surface_Equiv_Radius_OfModel;
			}
			return true;
		}

		internal static void OrientTheVertices(double JD, double dLambda_deg, double dBeta_deg)
		{
			double num = 24.0 / Period * 360.0;
			double num2 = (Phi0 + (JD - 0.00577551833 * MatrixOps.Modulus(EarthFromAsteroid) - JD0) * num + YORP * (JD - JD0) * (JD - JD0) / 2.0) % 360.0 / (180.0 / Math.PI);
			Matrix matrix = new Matrix();
			matrix.SetMatrixValues(Math.Cos(num2), 0.0 - Math.Sin(num2), 0.0, Math.Sin(num2), Math.Cos(num2), 0.0, 0.0, 0.0, 1.0);
			double num3 = (90.0 - Beta - dBeta_deg) / (180.0 / Math.PI);
			Matrix matrix2 = new Matrix();
			matrix2.SetMatrixValues(Math.Cos(num3), 0.0, Math.Sin(num3), 0.0, 1.0, 0.0, 0.0 - Math.Sin(num3), 0.0, Math.Cos(num3));
			double num4 = (Lambda + dLambda_deg) / (180.0 / Math.PI);
			Matrix matrix3 = new Matrix();
			matrix3.SetMatrixValues(Math.Cos(num4), 0.0 - Math.Sin(num4), 0.0, Math.Sin(num4), Math.Cos(num4), 0.0, 0.0, 0.0, 1.0);
			EquatorialVertices.Clear();
			for (int i = 0; i < Vertices.Count; i++)
			{
				Vector item = MatrixOps.Rotation(ToEquator, MatrixOps.Rotation(matrix3, MatrixOps.Rotation(matrix2, MatrixOps.Rotation(matrix, Vertices[i]))));
				EquatorialVertices.Add(item);
			}
			List<Vector> list = new List<Vector>();
			Vector vector = new Vector();
			vector.SetVectorValues(0.0, 0.0, MaxZ);
			list.Add(vector);
			vector = new Vector();
			vector.SetVectorValues(0.0, 0.0, 1.5 * MaxZ);
			list.Add(vector);
			vector = new Vector();
			vector.SetVectorValues(0.0, 0.0, MinZ);
			list.Add(vector);
			vector = new Vector();
			vector.SetVectorValues(0.0, 0.0, 1.5 * MinZ);
			list.Add(vector);
			PlotAxes.Clear();
			for (int j = 0; j < 4; j++)
			{
				Vector vector2 = new Vector();
				vector2 = MatrixOps.Rotation(ToEquator, MatrixOps.Rotation(matrix3, MatrixOps.Rotation(matrix2, MatrixOps.Rotation(matrix, list[j]))));
				Vector vector3 = new Vector();
				vector3.SetVectorValues(MatrixOps.DotProduct(Camera_Right, vector2), MatrixOps.DotProduct(Camera_Up, vector2), 0.0 - MatrixOps.DotProduct(Camera_To, vector2));
				PlotAxes.Add(vector3);
			}
			PlotVertices.Clear();
			for (int k = 0; k < Vertices.Count; k++)
			{
				Vector vector4 = new Vector();
				vector4.SetVectorValues(MatrixOps.DotProduct(Camera_Right, EquatorialVertices[k]), MatrixOps.DotProduct(Camera_Up, EquatorialVertices[k]), 0.0 - MatrixOps.DotProduct(Camera_To, EquatorialVertices[k]));
				PlotVertices.Add(vector4);
			}
			PlotData.Clear();
			for (int l = 0; l < Faces.Count; l++)
			{
				PlotCoords plotCoords = new PlotCoords();
				plotCoords.V1 = PlotVertices[Faces[l].F1];
				plotCoords.V2 = PlotVertices[Faces[l].F2];
				plotCoords.V3 = PlotVertices[Faces[l].F3];
				Vector v = MatrixOps.Subtraction(plotCoords.V2, plotCoords.V1);
				Vector v2 = MatrixOps.Subtraction(plotCoords.V3, plotCoords.V2);
				Vector r = MatrixOps.CrossProduct(v, v2);
				plotCoords.FaceNormal = MatrixOps.UnitVector(r);
				if (IsBinary && l > BinaryBoundary)
				{
					plotCoords.Body = 2;
				}
				PlotData.Add(plotCoords);
			}
			PlotData.Sort();
		}

		internal static void PlotShapeModel(Graphics formGraphics, float ChartWidth, float ChartHeight, bool BlackWhite, bool PlotModelDark, bool EarthPlane, Color Background, bool ShowFaceEdges, bool IncludeAxisOfRotation)
		{
			PlotShapeModel(formGraphics, ChartWidth, ChartHeight, BlackWhite, PlotModelDark, EarthPlane, Background, ShowFaceEdges, IncludeAxisOfRotation, useDamitV2imageScale: true);
		}

		internal static void PlotShapeModel(Graphics formGraphics, float ChartWidth, float ChartHeight, bool BlackWhiteModel, bool PlotModelDark, bool EarthPlane, Color Background, bool ShowFaceEdges, bool IncludeAxisOfRotation, bool useDamitV2imageScale)
		{
			int num = 0;
			int num2 = 255;
			int num3 = 200;
			int num4 = 200;
			int num5 = 200;
			if (useDamitV2imageScale)
			{
				num = 1;
			}
			Vol_Equiv_Radius_OnPlot = Volume_Equiv_Radius_OfModel * (double)ChartWidth * (double)ImagePlotScale_1Radius[num];
			Surf_Equiv_Radius_OnPlot = Surface_Equiv_Radius_OfModel * (double)ChartWidth * (double)ImagePlotScale_1Radius[num];
			formGraphics.Clear(Background);
			Pen pen = new Pen(Brushes.Black, 1f);
			Pen pen2 = new Pen(Brushes.White, 1f);
			Pen pen3 = new Pen(Brushes.Red, 4f);
			Pen pen4 = new Pen(Brushes.Blue, 4f);
			Brush brush = Brushes.Red;
			Brush brush2 = Brushes.Blue;
			if (BlackWhiteModel)
			{
				pen3 = new Pen(Brushes.Gray, 3f);
				pen4 = new Pen(Brushes.Gray, 3f);
				brush = Brushes.Black;
				brush2 = Brushes.Black;
			}
			_ = Brushes.Black;
			if (Background == Color.Black)
			{
				_ = Brushes.White;
			}
			double num6 = 1.0;
			if (EarthPlane)
			{
				num6 = -1.0;
			}
			double num7 = (0f - ImagePlotScale_1Radius[num]) * ChartWidth;
			float num8 = ChartWidth / 2f;
			float num9 = ChartHeight / 2f;
			PAofAxis = Math.Atan2(PlotAxes[0].X - PlotAxes[2].X, 0.0 - (PlotAxes[0].Y - PlotAxes[2].Y)) * (180.0 / Math.PI);
			double num10 = Math.Sqrt(Math.Pow(PlotAxes[0].X - PlotAxes[2].X, 2.0) + Math.Pow(PlotAxes[0].Y - PlotAxes[2].Y, 2.0));
			if (num10 < 0.0001)
			{
				num10 = 0.0001;
			}
			AngleOfAxisAboveSkyPlane = Math.Atan((PlotAxes[0].Z - PlotAxes[2].Z) / num10) * (180.0 / Math.PI);
			if (PAofAxis < 0.0)
			{
				PAofAxis += 360.0;
			}
			if (IncludeAxisOfRotation)
			{
				formGraphics.DrawLine(pen3, (float)((double)num8 + num6 * num7 * PlotAxes[0].X), (float)((double)num9 - num7 * PlotAxes[0].Y), (float)((double)num8 + num6 * num7 * PlotAxes[1].X), (float)((double)num9 - num7 * PlotAxes[1].Y));
				formGraphics.DrawString("N", new Font("Arial", 10f, FontStyle.Bold), brush, (float)((double)num8 + num6 * num7 * PlotAxes[0].X), (float)((double)num9 - num7 * PlotAxes[0].Y));
				formGraphics.DrawLine(pen4, (float)((double)num8 + num6 * num7 * PlotAxes[2].X), (float)((double)num9 - num7 * PlotAxes[2].Y), (float)((double)num8 + num6 * num7 * PlotAxes[3].X), (float)((double)num9 - num7 * PlotAxes[3].Y));
				formGraphics.DrawString("S", new Font("Arial", 10f, FontStyle.Bold), brush2, (float)((double)num8 + num6 * num7 * PlotAxes[2].X), (float)((double)num9 - num7 * PlotAxes[2].Y - 15.0));
			}
			for (int i = 0; i < PlotData.Count; i++)
			{
				if ((PlotData[i].FaceNormal.Z < 0.0) | double.IsNaN(PlotData[i].FaceNormal.Z))
				{
					continue;
				}
				Vector v = PlotData[i].V1;
				Vector v2 = PlotData[i].V2;
				Vector v3 = PlotData[i].V3;
				Point point = new Point((int)((double)num8 + num6 * num7 * v.X), (int)((double)num9 - num7 * v.Y));
				Point point2 = new Point((int)((double)num8 + num6 * num7 * v2.X), (int)((double)num9 - num7 * v2.Y));
				Point point3 = new Point((int)((double)num8 + num6 * num7 * v3.X), (int)((double)num9 - num7 * v3.Y));
				if (BlackWhiteModel)
				{
					num2 = (int)(120.0 * PlotData[i].FaceNormal.Z + 100.0);
					if (PlotModelDark)
					{
						num2 = (int)((double)num2 / 1.8);
					}
					if (PlotData[i].Body == 2)
					{
						num2 = (int)((double)num2 / 1.1);
					}
					num3 = (num4 = (num5 = num2));
				}
				else
				{
					num2 = (int)(150.0 * PlotData[i].FaceNormal.Z + 100.0);
					if (PlotModelDark)
					{
						num2 = (int)((double)num2 / 1.8);
					}
					if (PlotData[i].Body == 1)
					{
						num3 = num2;
						num4 = (int)((double)num3 / 1.09);
						num5 = (int)((double)num3 / 1.16);
					}
					else
					{
						num3 = (int)((double)num2 / 1.09);
						num4 = num2;
						num5 = (int)((double)num2 / 1.07);
					}
				}
				Brush brush3 = new SolidBrush(Color.FromArgb(255, num3, num4, num5));
				PointF[] points = new PointF[4] { point, point2, point3, point };
				formGraphics.FillPolygon(brush3, points);
				if (ShowFaceEdges)
				{
					int red = (int)((double)num3 / 1.15);
					int green = (int)((double)num4 / 1.15);
					int blue = (int)((double)num3 / 1.15);
					Pen pen5 = new Pen(Color.FromArgb(255, red, green, blue));
					formGraphics.DrawPolygon(pen5, points);
				}
			}
			if (IncludeAxisOfRotation)
			{
				if (PlotAxes[1].Z > 0.0)
				{
					formGraphics.DrawLine(pen3, (float)((double)num8 + num6 * num7 * PlotAxes[0].X), (float)((double)num9 - num7 * PlotAxes[0].Y), (float)((double)num8 + num6 * num7 * PlotAxes[1].X), (float)((double)num9 - num7 * PlotAxes[1].Y));
					formGraphics.DrawString("N", new Font("Arial", 10f, FontStyle.Bold), brush, (float)((double)num8 + num6 * num7 * PlotAxes[0].X), (float)((double)num9 - num7 * PlotAxes[0].Y));
				}
				else
				{
					formGraphics.DrawLine(pen4, (float)((double)num8 + num6 * num7 * PlotAxes[2].X), (float)((double)num9 - num7 * PlotAxes[2].Y), (float)((double)num8 + num6 * num7 * PlotAxes[3].X), (float)((double)num9 - num7 * PlotAxes[3].Y));
					formGraphics.DrawString("S", new Font("Arial", 10f, FontStyle.Bold), brush2, (float)((double)num8 + num6 * num7 * PlotAxes[2].X), (float)((double)num9 - num7 * PlotAxes[2].Y - 15.0));
				}
			}
			formGraphics.DrawEllipse(pen, num8 - 5f, num9 - 5f, 10f, 10f);
			formGraphics.DrawLine(pen, num8 - 5f, num9, num8 + 5f, num9);
			formGraphics.DrawLine(pen, num8, num9 - 5f, num8, num9 + 5f);
		}
	}
}
