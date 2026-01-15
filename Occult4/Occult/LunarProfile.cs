using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using Occult.Lunar_Observations;
using Occult.Mapping;
using Occult.Properties;

namespace Occult
{
	public class LunarProfile
	{
		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double MoonRadius = 0.2725076;

		public static string AppPath;

		internal static double LL;

		internal static double LB;

		internal static double AA;

		internal static double PAgraze;

		internal static double VPS_at_Moon;

		internal static double HorizontalScaleFactor;

		internal static double CA;

		internal static double Terminator_DistanceFromMoonCentre;

		internal static double PGraze;

		internal static double DGraze;

		internal static int NorthOrSouthLimit = 1;

		internal static int WaxWane = 1;

		internal static int AllCount = 0;

		internal static bool IsEclipse = false;

		internal static string MapTitle;

		internal static string PlotLocation;

		internal static string MapTitleWithProfile = "";

		internal static LunarGrazeProfile GrazeProfile;

		private static int GrazeDataType = 0;

		internal static int ElementRecord;

		private static float DegreeLen;

		private static float MapCenterX;

		private static float MapCenterY;

		private static float KmMileScale;

		private static float ArcSecLen;

		private static string KmMileTag;

		private static string LibrationRange;

		internal static float HeightScale = 1f;

		internal static float WidthScale = 1f;

		private static bool limitToGrazes = false;

		private static bool eventsInColour = true;

		private static bool drawInColor = false;

		private static bool isDouble = false;

		private static bool useLOLAHires = false;

		internal static bool generateHiresforEventCount = false;

		private static bool showLOLAPoints = false;

		internal static List<Points> KLimb = new List<Points>();

		private static double doublePA = 0.0;

		private static double doubleSep = 0.0;

		private static double meanRatio = 1.0;

		private static ArrayList DataFiles = new ArrayList();

		internal static List<LunarGrazeID> GrazeList = new List<LunarGrazeID>();

		private static LunarGrazeID GrazeID;

		internal static List<LunarObserver> ObserverList = new List<LunarObserver>();

		private static LunarObserver ObserversName;

		internal static ArrayList DistanceLines = new ArrayList();

		internal static List<HiResGrazeProfilePoint> HiResolutionGrazeProfile = new List<HiResGrazeProfilePoint>();

		public static int LibrationSetting
		{
			set
			{
				GrazeDataType = value;
			}
		}

		public static bool UseLOLAHires
		{
			get
			{
				return useLOLAHires;
			}
			set
			{
				useLOLAHires = value;
			}
		}

		public static bool ShowLOLAPoints
		{
			get
			{
				return showLOLAPoints;
			}
			set
			{
				showLOLAPoints = value;
			}
		}

		public static bool LimitToGrazes
		{
			get
			{
				return limitToGrazes;
			}
			set
			{
				limitToGrazes = value;
			}
		}

		public static bool DrawInColor
		{
			get
			{
				return drawInColor;
			}
			set
			{
				drawInColor = value;
			}
		}

		public static bool EventsInColour
		{
			get
			{
				return eventsInColour;
			}
			set
			{
				eventsInColour = value;
			}
		}

		public static bool IsDouble
		{
			get
			{
				return isDouble;
			}
			set
			{
				isDouble = value;
			}
		}

		public static double DoublePA
		{
			get
			{
				return doublePA;
			}
			set
			{
				doublePA = value;
			}
		}

		public static double DoubleSep
		{
			get
			{
				return doubleSep;
			}
			set
			{
				doubleSep = value;
			}
		}

		public static double MeanRatio
		{
			get
			{
				return meanRatio;
			}
			set
			{
				meanRatio = value;
			}
		}

		public static void InitialiseProfile()
		{
			try
			{
				((Control)GrazeProfile).Show();
			}
			catch
			{
				GrazeProfile = new LunarGrazeProfile();
				((Control)GrazeProfile).Show();
			}
			GetDataFiles();
		}

		internal static void SetLibrationDetails(int Record)
		{
			ElementRecord = Record;
			LL = LunarOccultations.GrazePredictionLines[Record].LL;
			LB = LunarOccultations.GrazePredictionLines[Record].LB;
			AA = LunarOccultations.GrazePredictionLines[Record].AA;
			PAgraze = LunarOccultations.GrazePredictionLines[Record].PA;
			Utilities.Librations_P_D(LL, LB, AA, out PGraze, out DGraze);
			VPS_at_Moon = GetTrueVerticalProfileScale(Record);
			HorizontalScaleFactor = LunarOccultations.GrazePredictionLines[Record].N * 3.5043;
			NorthOrSouthLimit = 1;
			if ((AA > 90.0) & (AA < 270.0))
			{
				NorthOrSouthLimit = -1;
			}
			CA = LunarOccultations.GrazePredictionLines[Record].CA;
			Terminator_DistanceFromMoonCentre = 1.0 - LunarOccultations.GrazePredictionLines[Record].Illumination / 50.0;
			IsEclipse = false;
			if (LunarOccultations.GrazePredictionLines[Record].WaxFlag == "+")
			{
				WaxWane = 1;
			}
			else if (LunarOccultations.GrazePredictionLines[Record].WaxFlag == "-")
			{
				WaxWane = -1;
			}
			else if (LunarOccultations.GrazePredictionLines[Record].WaxFlag == "E")
			{
				Terminator_DistanceFromMoonCentre = 1.0;
				WaxWane = 1;
				CA = 90.0;
				IsEclipse = true;
			}
			DistanceLines.Clear();
			string starId = LunarOccultations.GrazePredictionLines[Record].StarId;
			MapTitle = "Graze of  " + starId.Trim() + ",  Magnitude " + $"{LunarOccultations.GrazePredictionLines[Record].Mv:F1}" + $" [R {LunarOccultations.GrazePredictionLines[Record].Mr:F1}]" + ",  on " + Utilities.Date_from_JD(Math.Floor(LunarOccultations.GrazePredictionLines[Record].JD - 0.5) + 0.5, 0);
			PlotLocation = "For E. Long. " + $"{LunarOccultations.GrazePredictionLines[Record].GrazeLongitude:F1}";
		}

		internal static double GetTrueVerticalProfileScale(int Record)
		{
			if (Record < 1)
			{
				Record = 1;
			}
			double num;
			double num2;
			if (LunarOccultations.GrazePredictionLines[Record - 1].JD > LunarOccultations.GrazePredictionLines[Record].JD)
			{
				if (Record >= LunarOccultations.GrazePredictionLines.Count - 1)
				{
					Record--;
				}
				num = LunarOccultations.GrazePredictionLines[Record + 1].GrazeLongitude / (180.0 / Math.PI);
				num2 = LunarOccultations.GrazePredictionLines[Record + 1].GrazeLatitude / (180.0 / Math.PI);
			}
			else
			{
				num = LunarOccultations.GrazePredictionLines[Record - 1].GrazeLongitude / (180.0 / Math.PI);
				num2 = LunarOccultations.GrazePredictionLines[Record - 1].GrazeLatitude / (180.0 / Math.PI);
			}
			double num3 = LunarOccultations.GrazePredictionLines[Record].GrazeLongitude / (180.0 / Math.PI);
			double num4 = LunarOccultations.GrazePredictionLines[Record].GrazeLatitude / (180.0 / Math.PI);
			if ((Math.Abs(num3 - num) * (180.0 / Math.PI) * 60.0 < 1.0) & (Math.Abs(num4 - num2) * (180.0 / Math.PI) * 60.0 < 1.0))
			{
				return LunarOccultations.VPS_at_Moon;
			}
			Utilities.GetGeoidDistances(num, num2, num3, num4, out var _, out var _, out var AZ2);
			Utilities.GetGeoidLocation(num3, num4, 3.0 * LunarOccultations.VPS_at_Moon, AZ2 + Math.PI / 2.0, out var LongEnd, out var LatEnd);
			int result = 0;
			int elementRecordNumber = LunarOccultations.GrazePredictionLines[Record].ElementRecordNumber;
			string text;
			if (LunarOccultations.Elements[elementRecordNumber].PlanetId != "")
			{
				text = LunarOccultations.Elements[elementRecordNumber].PlanetId.Substring(0, 1);
				if (!int.TryParse(LunarOccultations.Elements[elementRecordNumber].PlanetId.Substring(1, 6), out result))
				{
					result = 6000;
				}
			}
			else
			{
				text = LunarOccultations.Elements[elementRecordNumber].StarId.PadRight(5).Substring(0, 1);
				if (LunarOccultations.Elements[elementRecordNumber].StarId.Substring(0, 3) == "   ")
				{
					text = "R";
				}
				else if (LunarOccultations.Elements[elementRecordNumber].StarId.Substring(0, 1) == " ")
				{
					text = "S";
				}
				if ("RSX".Contains(text) && !int.TryParse(LunarOccultations.Elements[elementRecordNumber].StarId.Substring(1, 6), out result))
				{
					result = 1;
				}
			}
			LunarObservations.ReduceAnObservation(LunarOccultations.GrazePredictionLines[Record].JD, LongEnd, LatEnd, LunarOccultations.GrazeNominalAltitude, Height_is_MSL: true, 84, text, result, UseOuterLimbOfSatelliteOrAsteroid: true, PE_ReductionIfBrightStar: false, "", ApplyLimbCorrn: false, VizierReduction: false, out var _, out var Residual, out var _, out var _, out var _, out var _, out var _, out var MoonRadius);
			double num5 = Math.Abs(Residual) / MoonRadius * 0.004521275427291311;
			return LunarOccultations.VPS_at_Moon * 3.0 / num5;
		}

		internal static void GetDataFiles()
		{
			DataFiles.Clear();
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "RArchive*.*");
			foreach (string path in files)
			{
				DataFiles.Add(Path.GetFileName(path));
			}
		}

		internal static void SaveGrazeProfile(int Width, int Height, string OutputFile, int FileType)
		{
			Application.DoEvents();
			Bitmap bitmap = new Bitmap(Width, Height);
			Graphics graphics = Graphics.FromImage(bitmap);
			DrawProfile(graphics, Width, Height, Printer: false, AutoGenerate: true, HighlightRePlot: false, "", "", "", ReReadHires: true);
			switch (FileType)
			{
			case 0:
				bitmap.Save(OutputFile + ".jpg", ImageFormat.Jpeg);
				break;
			case 1:
				bitmap.Save(OutputFile + ".bmp", ImageFormat.Bmp);
				break;
			case 2:
				bitmap.Save(OutputFile + ".gif", ImageFormat.Gif);
				break;
			case 3:
				bitmap.Save(OutputFile + ".png", ImageFormat.Png);
				break;
			case 4:
				bitmap.Save(OutputFile + ".tif", ImageFormat.Tiff);
				break;
			}
			graphics.Dispose();
		}

		internal static void PlotGrazeProfile(bool HighlightRePlot, string StarID, string ObserverID, string Date, bool ReReadHires)
		{
			Application.DoEvents();
			int width = ((Control)GrazeProfile.picProfile).get_Width();
			int height = ((Control)GrazeProfile.picProfile).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			DrawProfile(graphics, width, height, Printer: false, AutoGenerate: false, HighlightRePlot, StarID, ObserverID, Date, ReReadHires);
			GrazeProfile.picProfile.set_Image((Image)image);
			graphics.Dispose();
			((Control)GrazeProfile).Refresh();
		}

		internal static void PrintGrazeProfile()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintProfile;
				printDocument.Print();
			}
		}

		internal static void PrintPreviewGrazeProfile()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			PrintDocument printDocument = new PrintDocument();
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintProfile;
				((Form)val).ShowDialog();
			}
		}

		internal static void PrintProfile(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawProfile(graphics, chartWidth, chartHeight, Printer: true, AutoGenerate: false, HighlightRePlot: false, "", "", "", ReReadHires: true);
		}

		internal static void DrawProfile(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, bool AutoGenerate, bool HighlightRePlot, string StarID, string ObserverID, string Date, bool ReReadHires)
		{
			_ = new double[66];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			float num4 = 0f;
			float num5 = 0f;
			float num6 = 0f;
			float num7 = 0f;
			int num8 = 0;
			int num9 = 0;
			string text = "";
			bool flag = Settings.Default.BWFlag;
			if (AutoGenerate)
			{
				flag = !DrawInColor;
			}
			bool flag2 = false;
			GrazeProfile.PlotIsBeingDrawn = true;
			HiResolutionGrazeProfile.Clear();
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Arial", 7f);
			Font font2 = new Font("Arial", 8f);
			Font font3 = new Font("Arial", 12f, FontStyle.Bold);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Pen pen;
			Pen pen2;
			Pen pen3;
			Pen pen4;
			Pen pen5;
			Pen pen6;
			Pen pen7;
			Pen pen8;
			Pen pen9;
			Pen pen10;
			Pen pen11;
			Pen pen12;
			Pen pen13;
			Pen pen14;
			Brush brush;
			Brush brush2;
			Brush brush3;
			if (flag || Printer)
			{
				pen = new Pen(Brushes.Black, 1f);
				pen2 = new Pen(Color.FromArgb(150, Color.White), 5f);
				pen3 = new Pen(Color.FromArgb(150, Color.White), 2.5f);
				new Pen(Brushes.Black, 2f);
				pen4 = new Pen(Brushes.Black, 1f);
				pen5 = new Pen(Brushes.Black, 1f);
				pen6 = new Pen(Brushes.Black);
				if (Printer & EventsInColour)
				{
					pen7 = new Pen(Brushes.Red, 1f);
					pen8 = new Pen(Brushes.Lime, 1f);
					pen9 = new Pen(Brushes.Silver, 1f);
					pen10 = new Pen(Brushes.Blue, 2f);
					pen11 = new Pen(Brushes.LightBlue, 2f);
					pen12 = new Pen(Brushes.LightCoral, 2f);
					pen13 = new Pen(Color.FromArgb(200, 150, 150, 100));
					pen14 = new Pen(Brushes.Orange, 2f);
					brush = Brushes.Yellow;
				}
				else
				{
					pen7 = new Pen(Brushes.Black, 1f);
					pen8 = new Pen(Brushes.Black, 1f);
					pen9 = new Pen(Brushes.Black, 1f);
					pen10 = new Pen(Brushes.Black, 2f);
					pen11 = new Pen(Brushes.Black, 2f);
					pen12 = new Pen(Brushes.Black, 2f);
					pen13 = new Pen(Color.FromArgb(128, 0, 0, 0));
					pen14 = new Pen(Brushes.Black, 2f);
					brush = Brushes.Black;
				}
				brush2 = Brushes.Black;
				brush3 = Brushes.DarkGray;
			}
			else
			{
				pen = new Pen(Brushes.WhiteSmoke, 1f);
				pen2 = new Pen(Color.FromArgb(150, Color.Black), 5f);
				pen3 = new Pen(Color.FromArgb(150, Color.Black), 2.5f);
				new Pen(Brushes.WhiteSmoke, 2f);
				pen4 = new Pen(Brushes.Cyan, 0.1f);
				pen5 = new Pen(Brushes.Lime, 1f);
				pen6 = new Pen(Brushes.Orchid);
				pen13 = new Pen(Color.FromArgb(160, 150, 150, 100));
				pen14 = new Pen(Brushes.Orange, 2f);
				brush = Brushes.Yellow;
				if (!eventsInColour)
				{
					pen7 = new Pen(Brushes.White, 1f);
					pen8 = new Pen(Brushes.White, 1f);
					pen9 = new Pen(Brushes.White, 1f);
					pen10 = new Pen(Brushes.White, 2f);
					pen11 = new Pen(Brushes.White, 2f);
					pen12 = new Pen(Brushes.White, 2f);
				}
				else
				{
					pen7 = new Pen(Brushes.Red, 1f);
					pen8 = new Pen(Brushes.Lime, 1f);
					pen9 = new Pen(Brushes.Silver, 1f);
					pen10 = new Pen(Brushes.Coral, 2f);
					pen11 = new Pen(Brushes.LightBlue, 2f);
					pen12 = new Pen(Brushes.LightCoral, 2f);
				}
				brush2 = Brushes.White;
				brush3 = Brushes.CornflowerBlue;
			}
			pen5.DashStyle = DashStyle.Dash;
			pen6.DashPattern = new float[2] { 1f, 6f };
			float height = formGraphics.MeasureString("I", font3).Height;
			float height2 = formGraphics.MeasureString("I", font2).Height;
			Brush brush4;
			if (flag || Printer)
			{
				formGraphics.Clear(Color.White);
				brush4 = new SolidBrush(Color.FromArgb(140, Color.White));
			}
			else
			{
				formGraphics.Clear(Color.Black);
				brush4 = new SolidBrush(Color.FromArgb(140, Color.Black));
			}
			MapCenterX = (float)ChartWidth / 2f;
			float num10 = (float)ChartHeight / 2f;
			float num11 = 0.9f * MapCenterX;
			float num12 = 0.1f * num10;
			float num13 = 1.9f * num10;
			float num14 = MapCenterX - num11;
			float num15 = MapCenterX + num11;
			float num16 = 0.72f * num10;
			float num17 = 1.08f * num10;
			MapCenterY = num10 - (float)NorthOrSouthLimit * num16 / 4f;
			float num18 = num11 / 50f;
			DegreeLen = num11 / 5.8f;
			ArcSecLen = num16 / 3.2f;
			KmMileScale = 1f;
			KmMileTag = "km";
			if (!Settings.Default.GrazeProfile_in_km)
			{
				KmMileScale = 1.609f;
				KmMileTag = "mi";
			}
			if (HighlightRePlot & (LunarGrazeProfile.Tags.Count > 0))
			{
				for (int i = 0; i < LunarGrazeProfile.Tags.Count; i++)
				{
					if ((LunarGrazeProfile.Tags[i].StarID == StarID) | (LunarGrazeProfile.Tags[i].ObserverID == ObserverID) | (LunarGrazeProfile.Tags[i].Date == Date))
					{
						if (LunarGrazeProfile.Tags[i].EventType % 3 == 0)
						{
							formGraphics.DrawEllipse(pen10, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
						}
						else if (LunarGrazeProfile.Tags[i].EventType % 3 == 1)
						{
							formGraphics.DrawEllipse(pen11, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
						}
						else
						{
							formGraphics.DrawEllipse(pen12, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
						}
					}
					else if (LunarGrazeProfile.Tags[i].EventType % 3 == 0)
					{
						if (LunarGrazeProfile.Tags[i].EventType == 0)
						{
							formGraphics.DrawRectangle(pen7, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius);
							continue;
						}
						if (LunarGrazeProfile.Tags[i].EventType == 3)
						{
							formGraphics.DrawEllipse(pen7, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
							continue;
						}
						formGraphics.DrawLine(pen7, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y, LunarGrazeProfile.Tags[i].X + LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y);
						formGraphics.DrawLine(pen7, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y + LunarGrazeProfile.Tags[i].CircleRadius);
					}
					else if (LunarGrazeProfile.Tags[i].EventType % 3 == 1)
					{
						if (LunarGrazeProfile.Tags[i].EventType == 1)
						{
							formGraphics.DrawRectangle(pen8, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius);
							continue;
						}
						if (LunarGrazeProfile.Tags[i].EventType == 4)
						{
							formGraphics.DrawEllipse(pen8, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
							continue;
						}
						formGraphics.DrawLine(pen8, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y, LunarGrazeProfile.Tags[i].X + LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y);
						formGraphics.DrawLine(pen8, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y + LunarGrazeProfile.Tags[i].CircleRadius);
					}
					else if (LunarGrazeProfile.Tags[i].EventType == 2)
					{
						formGraphics.DrawRectangle(pen9, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius, 1.5f * LunarGrazeProfile.Tags[i].CircleRadius);
					}
					else if (LunarGrazeProfile.Tags[i].EventType == 5)
					{
						formGraphics.DrawEllipse(pen9, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius, 2f * LunarGrazeProfile.Tags[i].CircleRadius);
					}
					else
					{
						formGraphics.DrawLine(pen9, LunarGrazeProfile.Tags[i].X - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y, LunarGrazeProfile.Tags[i].X + LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].Y);
						formGraphics.DrawLine(pen9, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y - LunarGrazeProfile.Tags[i].CircleRadius, LunarGrazeProfile.Tags[i].X, LunarGrazeProfile.Tags[i].Y + LunarGrazeProfile.Tags[i].CircleRadius);
					}
				}
			}
			else
			{
				if (!Printer && !AutoGenerate)
				{
					LunarGrazeProfile.Tags.Clear();
					GrazeList.Clear();
					ObserverList.Clear();
				}
				num8 = 0;
				AllCount = 0;
				LibrationRange = "";
				if (GrazeDataType > 0)
				{
					double aA = AA;
					switch (GrazeDataType)
					{
					case 1:
						num = 0.3;
						LibrationRange = "P, D [+/- 0.3 deg]";
						break;
					case 2:
						num = 0.5;
						LibrationRange = "P, D [+/- 0.5 deg]";
						break;
					case 3:
						num = 1.0;
						LibrationRange = "P, D [+/- 1.0 deg]";
						break;
					case 4:
						num2 = 1.0;
						num3 = 0.5;
						LibrationRange = "AA, L [+/- 1 deg], B [+/- 0.5 deg]";
						break;
					case 5:
						num2 = 2.0;
						num3 = 0.5;
						LibrationRange = "AA, L [+/- 2 deg], B [+/- 0.5 deg]";
						break;
					case 6:
						num2 = 1.0;
						num3 = 1.0;
						LibrationRange = "AA, L [+/- 1 deg], B [+/- 1 deg]";
						break;
					case 7:
						num2 = 2.0;
						num3 = 1.0;
						LibrationRange = "AA, L [+/- 2 deg], B [+/- 1 deg]";
						break;
					case 8:
						num2 = 4.0;
						num3 = 1.0;
						LibrationRange = "AA, L [+/- 4 deg], B [+/- 1 deg]";
						break;
					case 9:
						num2 = 4.0;
						num3 = 2.0;
						LibrationRange = "AA, L [+/- 4 deg], B [+/- 2 deg]";
						break;
					}
					for (int j = 0; j < DataFiles.Count; j++)
					{
						StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + DataFiles[j]!.ToString());
						do
						{
							string text2 = streamReader.ReadLine();
							bool flag3 = false;
							if (text2.Length < 129)
							{
								continue;
							}
							string text3 = text2.Substring(60, 1);
							flag2 = text2.Substring(62, 1) == "G";
							bool flag4 = "GVP".Contains(text2.Substring(63, 1));
							if (!int.TryParse(text2.Substring(64, 1), out var result))
							{
								result = 0;
							}
							if ((((flag2 || flag4) | !limitToGrazes) && result <= 2) & (text3 != " "))
							{
								double result2;
								double result5;
								double result6;
								double result7;
								if (GrazeDataType > 3)
								{
									if (!double.TryParse(text2.Substring(97, 7), out result2))
									{
										result2 = 0.0;
									}
									result2 -= aA;
									if (result2 < -180.0)
									{
										result2 += 360.0;
									}
									if (result2 > 180.0)
									{
										result2 -= 360.0;
									}
									if (Math.Abs(result2) < 5.800000190734863)
									{
										if (!double.TryParse(text2.Substring(83, 8), out var result3))
										{
											result3 = 0.0;
										}
										if (!double.TryParse(text2.Substring(91, 6), out var result4))
										{
											result4 = 0.0;
										}
										if ((Math.Abs(LL - result3) < num2) & (Math.Abs(LB - result4) < num3))
										{
											if (!double.TryParse(text2.Substring(111, 6), out result5))
											{
												result5 = 1.0;
											}
											if (!double.TryParse(text2.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											if (!double.TryParse(text2.Substring(67, 8), out result7))
											{
												result7 = 0.0;
											}
											result6 /= result5;
											result7 /= result5;
											if (Math.Abs(result7) < 4.0)
											{
												num4 = (float)((double)MapCenterX - (double)((float)NorthOrSouthLimit * DegreeLen) * result2);
												num5 = (float)((double)MapCenterY - (double)((float)NorthOrSouthLimit * ArcSecLen) * (result7 - 932.58 * (1.0 - Math.Cos(result2 / (180.0 / Math.PI)))));
												float num19 = (float)(4 - result) / 1.8f;
												if (num19 < 1f)
												{
													num19 = 1f;
												}
												if (text3 == "D")
												{
													if (flag2)
													{
														formGraphics.DrawRectangle(pen7, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
														num9 = 0;
													}
													else if (flag4)
													{
														formGraphics.DrawEllipse(pen7, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
														num9 = 3;
													}
													else
													{
														formGraphics.DrawLine(pen7, num4 - num19, num5, num4 + num19, num5);
														formGraphics.DrawLine(pen7, num4, num5 + num19, num4, num5 - num19);
														num9 = 6;
													}
												}
												else if (text3 == "R")
												{
													if (flag2)
													{
														formGraphics.DrawRectangle(pen8, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
														num9 = 1;
													}
													else if (flag4)
													{
														formGraphics.DrawEllipse(pen8, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
														num9 = 4;
													}
													else
													{
														formGraphics.DrawLine(pen8, num4 - num19, num5, num4 + num19, num5);
														formGraphics.DrawLine(pen8, num4, num5 + num19, num4, num5 - num19);
														num9 = 7;
													}
												}
												else if (flag2)
												{
													formGraphics.DrawRectangle(pen9, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
													num9 = 2;
												}
												else if (flag4)
												{
													formGraphics.DrawEllipse(pen9, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
													num9 = 5;
												}
												else
												{
													formGraphics.DrawLine(pen9, num4 - num19, num5, num4 + num19, num5);
													formGraphics.DrawLine(pen9, num4, num5 + num19, num4, num5 - num19);
													num9 = 8;
												}
												AllCount++;
												if (!Printer && !AutoGenerate)
												{
													LunarGrazeTags lunarGrazeTags = new LunarGrazeTags();
													lunarGrazeTags.X = num4;
													lunarGrazeTags.Y = num5;
													lunarGrazeTags.CircleRadius = num19;
													lunarGrazeTags.EventType = num9;
													lunarGrazeTags.Date = text2.Substring(36, 11);
													lunarGrazeTags.ObserverID = text2.Substring(6, 20);
													lunarGrazeTags.StarID = text2.Substring(27, 8);
													if (!int.TryParse(text2.Substring(41, 2), out var result8))
													{
														result8 = 0;
													}
													lunarGrazeTags.Tag = text2.Substring(36, 5) + Utilities.ShortMonths[result8] + text2.Substring(43, 14) + ", " + text2.Substring(60, 2) + ", " + "  ok? ??".Substring(2 * result, 2) + "," + text2.Substring(5, 21).Trim() + ", " + text2.Substring(27, 1) + text2.Substring(28, 7).Trim();
													LunarGrazeProfile.Tags.Add(lunarGrazeTags);
													flag3 = true;
												}
											}
										}
									}
								}
								else
								{
									if (!double.TryParse(text2.Substring(117, 7), out result2))
									{
										result2 = 0.0;
									}
									result2 -= PGraze;
									if (result2 < -180.0)
									{
										result2 += 360.0;
									}
									if (result2 > 180.0)
									{
										result2 -= 360.0;
									}
									if (Math.Abs(result2) < 5.800000190734863)
									{
										if (!double.TryParse(text2.Substring(124, 6), out var result9))
										{
											result9 = 0.0;
										}
										if (Math.Abs(DGraze - result9) < num)
										{
											if (!double.TryParse(text2.Substring(111, 6), out result5))
											{
												result5 = 1.0;
											}
											if (!double.TryParse(text2.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											if (!double.TryParse(text2.Substring(67, 8), out result7))
											{
												result7 = 0.0;
											}
											result6 /= result5;
											result7 /= result5;
											if (Math.Abs(result7) < 4.0)
											{
												num4 = (float)((double)MapCenterX - (double)((float)NorthOrSouthLimit * DegreeLen) * result2);
												num5 = (float)((double)MapCenterY - (double)((float)NorthOrSouthLimit * ArcSecLen) * (result7 - 932.58 * (1.0 - Math.Cos(result2 / (180.0 / Math.PI)))));
												float num19 = (float)(4 - result) / 1.8f;
												if (num19 < 1f)
												{
													num19 = 1f;
												}
												if (text3 == "D")
												{
													if (flag2)
													{
														formGraphics.DrawRectangle(pen7, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
														num9 = 0;
													}
													else if (flag4)
													{
														formGraphics.DrawEllipse(pen7, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
														num9 = 3;
													}
													else
													{
														formGraphics.DrawLine(pen7, num4 - num19, num5, num4 + num19, num5);
														formGraphics.DrawLine(pen7, num4, num5 + num19, num4, num5 - num19);
														num9 = 6;
													}
												}
												else if (text3 == "R")
												{
													if (flag2)
													{
														formGraphics.DrawRectangle(pen8, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
														num9 = 1;
													}
													else if (flag4)
													{
														formGraphics.DrawEllipse(pen8, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
														num9 = 4;
													}
													else
													{
														formGraphics.DrawLine(pen8, num4 - num19, num5, num4 + num19, num5);
														formGraphics.DrawLine(pen8, num4, num5 + num19, num4, num5 - num19);
														num9 = 7;
													}
												}
												else if (flag2)
												{
													formGraphics.DrawRectangle(pen9, num4 - num19, num5 - num19, 1.5f * num19, 1.5f * num19);
													num9 = 2;
												}
												else if (flag4)
												{
													formGraphics.DrawEllipse(pen9, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
													num9 = 5;
												}
												else
												{
													formGraphics.DrawLine(pen9, num4 - num19, num5, num4 + num19, num5);
													formGraphics.DrawLine(pen9, num4, num5 + num19, num4, num5 - num19);
													num9 = 8;
												}
												AllCount++;
												if (!Printer && !AutoGenerate)
												{
													LunarGrazeTags lunarGrazeTags = new LunarGrazeTags();
													lunarGrazeTags.X = num4;
													lunarGrazeTags.Y = num5;
													lunarGrazeTags.CircleRadius = num19;
													lunarGrazeTags.EventType = num9;
													lunarGrazeTags.Date = text2.Substring(36, 11);
													lunarGrazeTags.ObserverID = text2.Substring(6, 20);
													lunarGrazeTags.StarID = text2.Substring(27, 8);
													if (!int.TryParse(text2.Substring(41, 2), out var result10))
													{
														result10 = 0;
													}
													lunarGrazeTags.Tag = text2.Substring(36, 5) + Utilities.ShortMonths[result10] + text2.Substring(43, 14) + ", " + text2.Substring(60, 2) + ", " + "  ok? ??".Substring(2 * result, 2) + "," + text2.Substring(5, 21).Trim() + ", " + text2.Substring(27, 1) + text2.Substring(28, 7).Trim();
													LunarGrazeProfile.Tags.Add(lunarGrazeTags);
													flag3 = true;
												}
											}
										}
									}
								}
							}
							if (flag3 && flag2)
							{
								string text4 = text2.Substring(36, 10) + text2.Substring(27, 8);
								if (text4 != text)
								{
									if (num8 > 0)
									{
										GrazeID.Count = num8;
										GrazeList.Add(GrazeID);
									}
									GrazeID = new LunarGrazeID();
									GrazeID.Date = text2.Substring(36, 10);
									GrazeID.StarID = text2.Substring(27, 8);
									GrazeID.L = text2.Substring(86, 4);
									GrazeID.B = text2.Substring(92, 4);
									text = text4;
									num8 = 1;
								}
								else
								{
									num8++;
								}
							}
							if (!flag3)
							{
								continue;
							}
							bool flag5 = false;
							string text5 = text2.Substring(6, 20);
							for (int k = 0; k < ObserverList.Count; k++)
							{
								if (ObserverList[k].Observer.ToString() == text5)
								{
									ObserverList[k].Count++;
									flag5 = true;
									break;
								}
							}
							if (!flag5)
							{
								ObserversName = new LunarObserver();
								ObserversName.Observer = text5;
								ObserversName.Count = 1;
								ObserverList.Add(ObserversName);
								ObserverList.Sort();
							}
						}
						while (!streamReader.EndOfStream);
						streamReader.Close();
					}
				}
				if (num8 > 0)
				{
					GrazeID.Count = num8;
					GrazeList.Add(GrazeID);
				}
				if (!Printer && !AutoGenerate)
				{
					LunarGrazeProfile.Tags.Sort();
					LunarGrazeID.SortField = 4;
					GrazeList.Sort();
				}
			}
			if (useLOLAHires | showLOLAPoints)
			{
				double num20 = AA - 6.0;
				double num21 = AA + 6.0;
				if (ReReadHires)
				{
					KLimb.Clear();
					if (useLOLAHires | showLOLAPoints)
					{
						LOLAHiRes.GetLimbData(LL, LB, num20, num21, 1.0, KLimb);
					}
				}
				if (showLOLAPoints)
				{
					float num19 = 1f;
					for (int l = 0; l < KLimb.Count; l++)
					{
						double num22 = KLimb[l].AA - AA;
						if (num22 > 180.0)
						{
							num22 -= 360.0;
						}
						if (num22 < -180.0)
						{
							num22 += 360.0;
						}
						num4 = (float)((double)(-NorthOrSouthLimit) * num22 * (double)DegreeLen + (double)MapCenterX);
						num5 = (float)((double)MapCenterY - (double)((float)NorthOrSouthLimit * ArcSecLen) * (KLimb[l].H - 932.58 * (1.0 - Math.Cos(num22 / (180.0 / Math.PI)))));
						if (num4 > num14 && num4 < num15 && num5 > num12 && num5 < num13)
						{
							if (KLimb[l].Valid)
							{
								formGraphics.DrawEllipse(pen13, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
							}
							else
							{
								formGraphics.DrawEllipse(pen5, num4 - num19, num5 - num19, 2f * num19, 2f * num19);
							}
						}
					}
				}
				if (useLOLAHires)
				{
					int num23 = 0;
					int NextLocation = 0;
					int NextLocation2 = 0;
					int NewStartLocation = 0;
					ArrayList arrayList = new ArrayList();
					while (!(KLimb[num23].AA > num20))
					{
						num23++;
						if (num23 >= KLimb.Count)
						{
							break;
						}
					}
					LOLA_HiRes.GetNewStartingPoint(KLimb, num23, out NextLocation);
					double num24;
					do
					{
						if (!LOLA_HiRes.GetNextWithSimilarD(KLimb, NextLocation, out NextLocation2))
						{
							LOLA_HiRes.GetNewStartingPoint(KLimb, NextLocation, out NextLocation2);
						}
						if (LOLA_HiRes.CheckForHigherPoints(KLimb, NextLocation, NextLocation2, out NewStartLocation))
						{
							NextLocation2 = NewStartLocation;
						}
						if (LOLA_HiRes.CheckForHigherPoints(KLimb, NextLocation, NextLocation2, out NewStartLocation))
						{
							NextLocation2 = NewStartLocation;
						}
						double num22 = KLimb[NextLocation].AA - AA;
						if (num22 > 180.0)
						{
							num22 -= 360.0;
						}
						if (num22 < -180.0)
						{
							num22 += 360.0;
						}
						num4 = (float)((double)(-NorthOrSouthLimit) * num22 * (double)DegreeLen + (double)MapCenterX);
						num5 = (float)((double)MapCenterY - (double)((float)NorthOrSouthLimit * ArcSecLen) * (KLimb[NextLocation].H - 932.58 * (1.0 - Math.Cos(num22 / (180.0 / Math.PI)))));
						if (generateHiresforEventCount)
						{
							HiResGrazeProfilePoint hiResGrazeProfilePoint = new HiResGrazeProfilePoint();
							hiResGrazeProfilePoint.AA = num22;
							hiResGrazeProfilePoint.Height = (KLimb[NextLocation].H - 932.58 * (1.0 - Math.Cos(num22 / (180.0 / Math.PI)))) * VPS_at_Moon;
							HiResolutionGrazeProfile.Add(hiResGrazeProfilePoint);
						}
						if (useLOLAHires)
						{
							if (num4 > num14 && num4 < num15 && num5 > num12 && num5 < num13)
							{
								arrayList.Add(new PointF(num4, num5));
							}
							if (arrayList.Count > 0)
							{
								PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
								if (arrayList.Count > 1)
								{
									formGraphics.DrawLines(pen4, points);
								}
							}
						}
						NextLocation = NextLocation2;
						num24 = KLimb[NextLocation2].AA;
						if (num24 < num20)
						{
							num24 += 360.0;
						}
					}
					while (num24 < num21);
				}
			}
			if (generateHiresforEventCount & (HiResolutionGrazeProfile.Count > 0))
			{
				double num25 = 0.2;
				double num26 = 0.1;
				if (Settings.Default.GrazeEventCount_HiRes)
				{
					num25 = 0.02;
					num26 = 0.01;
				}
				for (double num27 = -20.0; num27 <= 20.0; num27 += num25)
				{
					int num28 = 0;
					int num29 = HiResolutionGrazeProfile.Count - 1;
					double num30 = 0.0;
					num5 = (float)((double)((float)(-NorthOrSouthLimit) * ArcSecLen) * num27 / VPS_at_Moon + (double)MapCenterY);
					float num31 = (float)((double)ArcSecLen * 0.2 / VPS_at_Moon);
					if (!(num5 > num12 && num5 < num13))
					{
						continue;
					}
					num30 = HiResolutionGrazeProfile[0].Height - num27;
					if (num30 > 0.0)
					{
						num28++;
					}
					for (double num32 = -6.0; num32 < 6.0; num32 += num26)
					{
						int num33 = 0;
						while (HiResolutionGrazeProfile[num33].AA < num32)
						{
							num33++;
							if (num33 >= num29)
							{
								break;
							}
						}
						double num34 = HiResolutionGrazeProfile[num33].Height - num27;
						if (num30 < 0.0 && num34 > 0.0)
						{
							num28++;
						}
						num30 = num34;
					}
					formGraphics.FillRectangle(brush3, num14 + 2f, num5 - num31 / 2f, 5f * (float)num28, num31);
					if (num27 > 0.0 && num28 == 0)
					{
						break;
					}
				}
			}
			if (showLOLAPoints | useLOLAHires)
			{
				MapTitleWithProfile = MapTitle + "  :  LRO LOLA";
			}
			float num35 = formGraphics.MeasureString(MapTitleWithProfile, font3).Width / 2f;
			height = formGraphics.MeasureString(MapTitleWithProfile, font3).Height;
			formGraphics.FillRectangle(brush4, MapCenterX - num35 - 2f, MapCenterY + (float)NorthOrSouthLimit * 0.9f * num17 - 1f, num35 * 2f + 4f, height + 2f);
			formGraphics.DrawString(MapTitleWithProfile, font3, brush2, MapCenterX - num35, MapCenterY + (float)NorthOrSouthLimit * 0.9f * num17);
			num35 = formGraphics.MeasureString(PlotLocation, font3).Width / 2f;
			formGraphics.DrawString(PlotLocation, font3, brush2, MapCenterX - num35, MapCenterY - (float)NorthOrSouthLimit * 0.9f * num16);
			height = formGraphics.MeasureString("I", font3).Height;
			height2 = formGraphics.MeasureString("I", font2).Height;
			string text6;
			if (GrazeDataType > 0)
			{
				text6 = "D";
				num4 = MapCenterX - formGraphics.MeasureString(text6, font2).Width;
				num5 = ((NorthOrSouthLimit <= 0) ? ((float)((double)MapCenterY - 0.9 * (double)num16 - (double)height + (double)(2f * height2))) : ((float)((double)MapCenterY + 0.9 * (double)num16 + (double)height - (double)(2f * height2))));
				formGraphics.DrawString(text6, font2, brush2, num4, num5);
				formGraphics.DrawEllipse(pen7, MapCenterX + 5f, num5 + height2 / 2f, 3f, 3f);
				formGraphics.DrawRectangle(pen7, MapCenterX + 15f, num5 + height2 / 2f, 3f, 3f);
				formGraphics.DrawLine(pen7, MapCenterX + 23.5f, num5 + height2 / 2f + 1.5f, MapCenterX + 26.5f, num5 + height2 / 2f + 1.5f);
				formGraphics.DrawLine(pen7, MapCenterX + 25f, num5 + height2 / 2f, MapCenterX + 25f, num5 + height2 / 2f + 3f);
				text6 = "R";
				num4 = MapCenterX - formGraphics.MeasureString(text6, font2).Width;
				num5 = ((NorthOrSouthLimit <= 0) ? ((float)((double)MapCenterY - 0.9 * (double)num16 - (double)height + (double)(3f * height2))) : ((float)((double)MapCenterY + 0.9 * (double)num16 + (double)height - (double)height2)));
				formGraphics.DrawString(text6, font2, brush2, num4, num5);
				formGraphics.DrawEllipse(pen8, MapCenterX + 5f, num5 + height2 / 2f, 3f, 3f);
				formGraphics.DrawRectangle(pen8, MapCenterX + 15f, num5 + height2 / 2f, 3f, 3f);
				formGraphics.DrawLine(pen8, MapCenterX + 23.5f, num5 + height2 / 2f + 1.5f, MapCenterX + 26.5f, num5 + height2 / 2f + 1.5f);
				formGraphics.DrawLine(pen8, MapCenterX + 25f, num5 + height2 / 2f, MapCenterX + 25f, num5 + height2 / 2f + 3f);
			}
			float num36 = (float)((double)MapCenterX + (double)DegreeLen * CA * (double)WaxWane);
			float y = (float)((double)MapCenterY + (double)((float)NorthOrSouthLimit * ArcSecLen) * 932.58 * (1.0 - Math.Cos(CA / (180.0 / Math.PI))));
			for (int m = -29; (float)m <= 29f; m++)
			{
				num5 = (float)((double)MapCenterY + (double)((float)NorthOrSouthLimit * ArcSecLen) * 932.58 * (1.0 - Math.Cos((double)m / 5.0 / (180.0 / Math.PI))));
				num4 = MapCenterX + (float)(NorthOrSouthLimit * m) * DegreeLen / 5f;
				formGraphics.FillEllipse(brush4, num4 - 2f, num5 - 2f, 4f, 4f);
				formGraphics.FillEllipse(brush2, num4 - 1f, num5 - 1f, 2f, 2f);
				if (((float)m > -29f) & !IsEclipse)
				{
					if (Math.Sign(num4 - num36) != Math.Sign(num6 - num36))
					{
						if (((NorthOrSouthLimit == -1) & (WaxWane == 1)) | ((NorthOrSouthLimit == 1) & (WaxWane == -1)))
						{
							formGraphics.DrawLine(pen2, num6, num7, num36, y);
							formGraphics.DrawLine(pen, num6, num7, num36, y);
						}
						else
						{
							formGraphics.DrawLine(pen2, num4, num5, num36, y);
							formGraphics.DrawLine(pen, num4, num5, num36, y);
						}
					}
					else if ((WaxWane == 1 && num4 > num36) | (WaxWane == -1 && num4 < num36))
					{
						formGraphics.DrawLine(pen2, num6, num7, num4, num5);
						formGraphics.DrawLine(pen, num6, num7, num4, num5);
					}
				}
				num6 = num4;
				num7 = num5;
			}
			if (!IsEclipse)
			{
				for (float num37 = 0f; num37 < 100f; num37 += 0.2f)
				{
					float num38 = (float)((double)(-NorthOrSouthLimit * WaxWane) * Math.Atan(Terminator_DistanceFromMoonCentre * Math.Tan((double)num37 / (180.0 / Math.PI))) * (180.0 / Math.PI));
					if (!((float)Math.Abs((double)WaxWane * CA + (double)num38) < 17f))
					{
						continue;
					}
					float num39 = (float)(932.58 * (1.0 - Math.Sqrt(Math.Pow(Math.Cos((double)num37 / (180.0 / Math.PI)), 2.0) + Math.Pow(Terminator_DistanceFromMoonCentre * Math.Sin((double)num37 / (180.0 / Math.PI)), 2.0))));
					num4 = (float)((double)MapCenterX - (double)((float)NorthOrSouthLimit * DegreeLen) * ((double)num38 - (double)(NorthOrSouthLimit * WaxWane) * CA));
					float num40 = (float)Math.Cos(((double)num38 - (double)(NorthOrSouthLimit * WaxWane) * CA) / (180.0 / Math.PI));
					num5 = (float)((double)MapCenterY + (double)((float)NorthOrSouthLimit * ArcSecLen) * ((double)num39 + 932.58 * (double)(1f - num40)));
					if (num37 > 0f)
					{
						if (num7 > num13 && num5 < num13)
						{
							num6 += (num4 - num6) / (num5 - num7) * (num13 - num7);
							num7 = num13;
						}
						if (num5 > num13 && num7 < num13)
						{
							num4 = num6 + (num4 - num6) / (num5 - num7) * (num13 - num7);
							num5 = num13;
						}
						if (num7 < num12 && num5 > num12)
						{
							num6 += (num4 - num6) / (num5 - num7) * (num12 - num7);
							num7 = num12;
						}
						if (num5 < num12 && num7 > num12)
						{
							num4 = num6 + (num4 - num6) / (num5 - num7) * (num12 - num7);
							num5 = num12;
						}
						if (num6 > num14 && num4 < num15 && (num5 >= num12 || num7 >= num12) && (num7 <= num13 || num5 <= num13))
						{
							formGraphics.DrawLine(pen2, num6, num7, num4, num5);
							formGraphics.DrawLine(pen, num6, num7, num4, num5);
						}
						if ((num4 <= num14 && num6 > num4) || (num4 >= num15 && num6 < num4) || (num5 <= num12 && num7 > num5) || (num5 >= num13 && num7 < num5))
						{
							break;
						}
					}
					num6 = num4;
					num7 = num5;
				}
			}
			for (int n = 0; n < DistanceLines.Count; n++)
			{
				num5 = (float)((double)MapCenterY - (double)Convert.ToSingle(DistanceLines[n]!.ToString()) / VPS_at_Moon * (double)ArcSecLen * (double)KmMileScale * (double)NorthOrSouthLimit);
				formGraphics.DrawLine(pen3, num14, num5, num15, num5);
				formGraphics.DrawLine(pen6, num14, num5, num15, num5);
				text6 = $"{Convert.ToSingle(DistanceLines[n]!.ToString()):+#0.00;-#0.00}" + KmMileTag;
				num35 = formGraphics.MeasureString(text6, font2).Width;
				height = formGraphics.MeasureString(text6, font2).Height;
				formGraphics.FillRectangle(brush4, num15 - 50f - num35, num5 - height / 2f, num35, height);
				formGraphics.DrawString(text6, font2, brush2, num15 - 50f - num35, num5 - height / 2f);
			}
			num5 = ((NorthOrSouthLimit != 1) ? (num13 - num12) : (2f * num12));
			formGraphics.DrawLine(pen, num14 + DegreeLen, num5 - num18, num14 + DegreeLen, num5 + num18);
			formGraphics.DrawLine(pen, num14 + 2f * DegreeLen, num5 - num18, num14 + 2f * DegreeLen, num5 + num18);
			formGraphics.DrawLine(pen, num14 + DegreeLen, num5, num14 + 2f * DegreeLen, num5);
			text6 = "1 Deg.";
			num35 = formGraphics.MeasureString(text6, font2).Width / 2f;
			formGraphics.DrawString(text6, font2, brush2, num14 + 1.5f * DegreeLen - num35, num5 - num18 - 12f);
			formGraphics.DrawLine(pen2, num14, MapCenterY, num15, MapCenterY);
			for (int num41 = -5; num41 <= 5; num41++)
			{
				text6 = $"{num41:+0;-0}m";
				num35 = formGraphics.MeasureString(text6, font2).Width / 2f;
				num4 = (float)((double)MapCenterX + (double)num41 * HorizontalScaleFactor * (double)DegreeLen);
				if (num4 > num14 && num4 < num15)
				{
					formGraphics.DrawLine(pen2, num4, num12, num4, num12 + num18);
					formGraphics.DrawLine(pen, num4, num12, num4, num12 + num18);
					formGraphics.FillRectangle(brush4, num4 - num35, num12 + num18 + 1f, num35 * 2f, formGraphics.MeasureString(text6, font2).Height);
					formGraphics.DrawString(text6, font2, brush2, num4 - num35, num12 + num18 + 1f);
					formGraphics.DrawLine(pen2, num4, MapCenterY - num18, num4, MapCenterY + num18);
					formGraphics.DrawLine(pen, num4, MapCenterY - num18 - 3f, num4, MapCenterY + num18 + 3f);
					formGraphics.DrawLine(pen2, num4, num13, num4, num13 - num18);
					formGraphics.DrawLine(pen, num4, num13, num4, num13 - num18);
					formGraphics.FillRectangle(brush4, num4 - num35, num13 - num18 - 1f - formGraphics.MeasureString(text6, font2).Height, num35 * 2f, formGraphics.MeasureString(text6, font2).Height);
					formGraphics.DrawString(text6, font2, brush2, num4 - num35, num13 - num18 - 1f - formGraphics.MeasureString(text6, font2).Height);
				}
			}
			formGraphics.DrawLine(pen, 2f * num14 + num18, num10 - (float)NorthOrSouthLimit * num12 * 4f, 2f * num14, num10 - (float)NorthOrSouthLimit * num12 * 4f);
			formGraphics.DrawLine(pen, 2f * num14, num10 - (float)NorthOrSouthLimit * num12 * 4f, 2f * num14, num10 - (float)NorthOrSouthLimit * (num12 * 4f + ArcSecLen));
			formGraphics.DrawLine(pen, 2f * num14, num10 - (float)NorthOrSouthLimit * (num12 * 4f + ArcSecLen), 2f * num14 + num18, num10 - (float)NorthOrSouthLimit * (num12 * 4f + ArcSecLen));
			formGraphics.DrawString("1\"", font2, brush2, 2.2f * num14, num10 - (float)NorthOrSouthLimit * (num12 * 4f + ArcSecLen / 2f) - 6f);
			formGraphics.DrawString(string.Format("(VPS={0,1:f3})", VPS_at_Moon), font2, brush2, 2.2f * num14, num10 - (float)NorthOrSouthLimit * (num12 * 4f + ArcSecLen) + 2f);
			for (int num42 = -20; num42 <= 20; num42++)
			{
				num5 = (float)((double)(KmMileScale * ArcSecLen * (float)num42) / VPS_at_Moon + (double)MapCenterY);
				if (((num5 > num12) & (num5 < num13 - 2f * formGraphics.MeasureString("I", font3).Height)) && num42 != 0)
				{
					text6 = $"{-num42 * NorthOrSouthLimit:+#0;-#0}" + KmMileTag;
					num35 = formGraphics.MeasureString(text6, font2).Width;
					formGraphics.DrawLine(pen2, num14, num5, num14 + num18, num5);
					formGraphics.DrawLine(pen, num14, num5, num14 + num18, num5);
					formGraphics.FillRectangle(brush4, num14 + num18 + 2f, num5 - 6f, formGraphics.MeasureString(text6, font2).Width, formGraphics.MeasureString(text6, font2).Height);
					formGraphics.DrawString(text6, font2, brush2, num14 + num18 + 2f, num5 - 6f);
					formGraphics.DrawLine(pen2, num15, num5, num15 - num18, num5);
					formGraphics.DrawLine(pen, num15, num5, num15 - num18, num5);
					formGraphics.FillRectangle(brush4, num15 - num35 - num18 - 2f, num5 - 6f, formGraphics.MeasureString(text6, font2).Width, formGraphics.MeasureString(text6, font2).Height);
					formGraphics.DrawString(text6, font2, brush2, num15 - num35 - num18 - 2f, num5 - 6f);
				}
			}
			formGraphics.DrawRectangle(pen, num14, num12, 2f * num11, 0.9f * (float)ChartHeight);
			formGraphics.DrawLine(pen, num14, MapCenterY, num15, MapCenterY);
			if (isDouble)
			{
				double num43 = doublePA - PAgraze;
				if (NorthOrSouthLimit == 1)
				{
					num43 -= 180.0;
				}
				if (Math.Abs(doubleSep * Math.Cos(num43 / (180.0 / Math.PI))) < 3.0)
				{
					double num44 = LunarOccultations.PiMoon[12] / 0.015793;
					float num45 = (float)(doubleSep * Math.Sin(num43 / (180.0 / Math.PI)) * (double)DegreeLen / 16.3 / num44);
					float num46 = (float)(doubleSep * Math.Cos(num43 / (180.0 / Math.PI)) * (double)ArcSecLen / num44);
					float num47 = (float)(1.0 - meanRatio) * num45;
					float num48 = (float)(1.0 - meanRatio) * num46;
					num45 = (float)meanRatio * num45;
					num46 = (float)meanRatio * num46;
					formGraphics.DrawLine(pen2, MapCenterX - num47, MapCenterY - num48, MapCenterX + num45, MapCenterY + num46);
					formGraphics.DrawLine(pen14, MapCenterX - num47, MapCenterY - num48, MapCenterX + num45, MapCenterY + num46);
					formGraphics.FillEllipse(brush4, MapCenterX + num45 - 8f, MapCenterY + num46 - 3.5f, 8f, 8f);
					formGraphics.FillEllipse(brush, MapCenterX + num45 - 3f, MapCenterY + num46 - 3f, 6f, 6f);
					formGraphics.FillEllipse(brush4, MapCenterX - num47 - 4f, MapCenterY - num48 - 4f, 8f, 8f);
					formGraphics.FillEllipse(brush, MapCenterX - num47 - 3f, MapCenterY - num48 - 3f, 6f, 6f);
				}
			}
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush2, 3f, 3f);
			if (LibrationRange.Length > 0)
			{
				num4 = MapCenterX - formGraphics.MeasureString(LibrationRange, font2).Width / 2f;
				formGraphics.DrawString(y: (NorthOrSouthLimit <= 0) ? ((float)((double)MapCenterY - 0.9 * (double)num16 - (double)height + (double)height2)) : ((float)((double)MapCenterY + 0.9 * (double)num16 + (double)height - (double)(3f * height2))), s: LibrationRange, font: font2, brush: brush2, x: num4);
			}
			GrazeProfile.PlotIsBeingDrawn = false;
			Cursor.set_Current(Cursors.get_Default());
		}

		internal static string ToolTipText(float x, float y)
		{
			return "T = " + Utilities.DEGtoDMS((double)(x - MapCenterX) / HorizontalScaleFactor / (double)DegreeLen, 0, 1, MinutesOnly: true) + ",  D = " + string.Format("{0:F2}", (double)(MapCenterY - y) * VPS_at_Moon / (double)ArcSecLen / (double)KmMileScale * (double)NorthOrSouthLimit, 1) + KmMileTag;
		}

		internal static double CurrentCursorHeight(float y)
		{
			return (double)(MapCenterY - y) * VPS_at_Moon / (double)ArcSecLen / (double)KmMileScale * (double)NorthOrSouthLimit;
		}
	}
}
