using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Lunar_Observations;
using Occult.Mapping;
using Occult.Properties;

namespace Occult
{
	public class ReductionProfile
	{
		internal static bool IsDrawing = false;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double MoonRadius = 0.2725076;

		public static string AppPath;

		internal static double l;

		internal static double b;

		internal static double D;

		internal static double AA_Max;

		internal static double AA_Min;

		internal static double PMax;

		internal static double PMin;

		internal static double HeightMax;

		internal static double HeightMin;

		internal static double h_Max;

		internal static double h_Min;

		internal static double MoonScale = 1.0;

		internal static double AAforSlope;

		internal static double StarSepForPlot = 0.0;

		internal static double StarPAforPlot = 0.0;

		private static bool StarSepSet = false;

		internal static float VerticalScale_sec;

		internal static float HorizontalScale_deg;

		private static ArrayList DataFiles = new ArrayList();

		internal static List<LunarGrazeID> GrazeList = new List<LunarGrazeID>();

		internal static LunarGrazeID GrazeID;

		internal static List<LunarObserver> ObserverList = new List<LunarObserver>();

		internal static LunarObserver ObserversName;

		internal static List<Points> KLimb = new List<Points>();

		internal static double CurrentAAofMouse = 0.0;

		internal static Bitmap image;

		internal static string MapTitle;

		private static string LibrationRange = "";

		internal static ReductionPlot ObservedProfile;

		private static int GrazeDataType = 0;

		private static int SingleEvent_Number = -1;

		internal static int AllCount = 0;

		internal static float MapCenterX;

		internal static float MapCenterY;

		internal static float MapLeft;

		internal static float MapRight;

		private static bool limitToGrazes = false;

		private static bool eventsInColour = true;

		private static bool ShowSlopeLines = false;

		private static bool use_LOLA_HiRes = false;

		private static bool showLOLAPoints = false;

		public static int LibrationSetting
		{
			set
			{
				GrazeDataType = value;
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

		public static bool Use_LOLA_HiRes
		{
			get
			{
				return use_LOLA_HiRes;
			}
			set
			{
				use_LOLA_HiRes = value;
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

		public static void Show_ReductionProfile(int SinglePointLine)
		{
			try
			{
				((Control)ObservedProfile).Show();
			}
			catch
			{
				ObservedProfile = new ReductionPlot();
				((Control)ObservedProfile).Show();
			}
			((Control)ObservedProfile).Focus();
			SingleEvent_Number = SinglePointLine;
			if (GetPlotDetails())
			{
				ObservedProfile.DrawProfile(ReReadHiresData: true);
			}
		}

		internal static bool GetPlotDetails()
		{
			//IL_03a9: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			int num2 = 0;
			ShowSlopeLines = false;
			MoonScale = 1.0;
			StarSepSet = false;
			NumericUpDown updn_Sep = ObservedProfile.updn_Sep;
			NumericUpDown updn_PA = ObservedProfile.updn_PA;
			decimal value = default(decimal);
			updn_PA.set_Value(value);
			updn_Sep.set_Value(value);
			((Control)ObservedProfile.lblDblSep).set_Text("Sepn");
			((Control)ObservedProfile.lblDblPA).set_Text("PA");
			StarSepSet = false;
			if (SingleEvent_Number != -3)
			{
				l = (b = (D = 0.0));
				AA_Max = (PMax = (h_Max = -200.0));
				AA_Min = (PMin = (h_Min = 400.0));
				MapTitle = "";
			}
			if (SingleEvent_Number < 0)
			{
				if (SingleEvent_Number == -1)
				{
					for (int i = 0; i < LunarObservations.Residuals.Count; i++)
					{
						if (!(LunarObservations.Residuals[i].GrazeFlag == "G") || " MSEO".IndexOf(LunarObservations.Residuals[i].EventPhase) >= 0 || !(Math.Abs(LunarObservations.Residuals[i].O_C + LunarObservations.Residuals[i].Limb) < 3.5))
						{
							continue;
						}
						l += LunarObservations.Residuals[i].l;
						b += LunarObservations.Residuals[i].b;
						D += LunarObservations.Residuals[i].D;
						MoonScale = LunarObservations.Residuals[i].Scale;
						double num3 = LunarObservations.Residuals[i].AA;
						double num4 = LunarObservations.Residuals[i].P;
						if (num2 == 0)
						{
							num2 = ((!(num3 > 90.0 && num3 < 270.0)) ? 1 : (-1));
						}
						if (num2 == 1)
						{
							if (num3 < 90.0)
							{
								num3 += 360.0;
							}
							if (num4 < 90.0)
							{
								num4 += 360.0;
							}
						}
						if (num3 < AA_Min)
						{
							AA_Min = num3;
						}
						if (num3 > AA_Max)
						{
							AA_Max = num3;
						}
						if (num4 < PMin)
						{
							PMin = num4;
						}
						if (num4 > PMax)
						{
							PMax = num4;
						}
						double num5 = LunarObservations.Residuals[i].O_C + LunarObservations.Residuals[i].Limb;
						if (num5 > h_Max)
						{
							h_Max = num5;
						}
						if (num5 < h_Min)
						{
							h_Min = num5;
						}
						num++;
						if (MapTitle.Length < 1)
						{
							StringBuilder stringBuilder = new StringBuilder();
							stringBuilder.Append("Graze of  ");
							stringBuilder.Append(LunarObservations.Residuals[i].StarCat_Number);
							stringBuilder.Append("  on  ");
							stringBuilder.Append(LunarObservations.Residuals[i].Date);
							stringBuilder.Append("    L = ");
							stringBuilder.AppendFormat("{0,1:F2}", l);
							stringBuilder.Append("   B = ");
							stringBuilder.AppendFormat("{0,1:F2}", b);
							MapTitle = stringBuilder.ToString();
						}
					}
					if (num < 1)
					{
						MessageBox.Show("No graze events to plot", "No grazes", (MessageBoxButtons)0, (MessageBoxIcon)48);
						return false;
					}
				}
				else if (SingleEvent_Number == -3)
				{
					h_Max = 3.5;
					h_Min = -3.5;
					num2 = 0;
					num = 1;
					PMax = AA_Max;
					PMin = AA_Min;
					MapTitle = "Measure slope at Axis Angle " + string.Format("{0,1:f2}", AAforSlope);
					ShowSlopeLines = true;
				}
				else
				{
					l = 0.0;
					b = 0.0;
					D = 0.0;
					AA_Min = 86.0;
					AA_Max = 88.0;
					PMin = 86.0;
					PMax = 88.0;
					h_Max = 3.5;
					h_Min = -3.5;
					num2 = 0;
					num = 1;
					StringBuilder stringBuilder2 = new StringBuilder();
					stringBuilder2.Append("Occultation observations");
					MapTitle = stringBuilder2.ToString();
				}
			}
			else
			{
				l += LunarObservations.Residuals[SingleEvent_Number].l;
				b += LunarObservations.Residuals[SingleEvent_Number].b;
				D += LunarObservations.Residuals[SingleEvent_Number].D;
				MoonScale = LunarObservations.Residuals[SingleEvent_Number].Scale;
				double num3 = LunarObservations.Residuals[SingleEvent_Number].AA;
				double num4 = LunarObservations.Residuals[SingleEvent_Number].P;
				AA_Min = num3 - 1.0;
				AA_Max = num3 + 1.0;
				PMin = num4 - 1.0;
				PMax = num4 + 1.0;
				num2 = 0;
				double num5 = LunarObservations.Residuals[SingleEvent_Number].O_C + LunarObservations.Residuals[SingleEvent_Number].Limb;
				h_Max = num5 + 1.0;
				h_Min = num5 - 1.0;
				num = 1;
				StringBuilder stringBuilder3 = new StringBuilder();
				stringBuilder3.Append("Occultation of  ");
				stringBuilder3.Append(LunarObservations.Residuals[SingleEvent_Number].StarCat_Number);
				stringBuilder3.Append("  on  ");
				stringBuilder3.Append(LunarObservations.Residuals[SingleEvent_Number].Date + LunarObservations.Residuals[SingleEvent_Number].EventTime + ",");
				stringBuilder3.Append("  L = ");
				stringBuilder3.AppendFormat("{0,1:F2}", l);
				stringBuilder3.Append("  B = ");
				stringBuilder3.AppendFormat("{0,1:F2}", b);
				MapTitle = stringBuilder3.ToString();
			}
			l /= num;
			b /= num;
			D /= num;
			AA_Min -= 0.2;
			AA_Max += 0.2;
			if (num2 == 1)
			{
				AA_Min -= 360.0;
				AA_Max -= 360.0;
				PMin -= 360.0;
				PMax -= 360.0;
			}
			if ((AA_Min < 0.0) & (AA_Max < 0.0))
			{
				AA_Min += 360.0;
				AA_Max += 360.0;
			}
			ReductionPlot.Updating = true;
			ObservedProfile.updnLeft.set_Value((decimal)AA_Min);
			ObservedProfile.updnRight.set_Value((decimal)AA_Max);
			HeightMax = Math.Ceiling(2.0 * h_Max + 0.5) / 2.0;
			if ((HeightMax > 3.5) | (HeightMax < -3.0))
			{
				HeightMax = 3.5;
			}
			ObservedProfile.updnTop.set_Value((decimal)HeightMax);
			HeightMin = Math.Floor(2.0 * h_Min - 0.5) / 2.0;
			if ((HeightMin < -3.5) | (HeightMin > 3.0))
			{
				HeightMin = -3.5;
			}
			ObservedProfile.updnBottom.set_Value((decimal)HeightMin);
			ObservedProfile.updnL.set_Value((decimal)l);
			ObservedProfile.updnB.set_Value((decimal)b);
			ObservedProfile.updnD.set_Value((decimal)D);
			ObservedProfile.updnScale.set_Value((decimal)MoonScale);
			NumericUpDown updnL = ObservedProfile.updnL;
			NumericUpDown updnB = ObservedProfile.updnB;
			bool flag;
			((Control)ObservedProfile.updnScale).set_Enabled(flag = SingleEvent_Number < -1);
			bool enabled;
			((Control)updnB).set_Enabled(enabled = flag);
			((Control)updnL).set_Enabled(enabled);
			ReductionPlot.Updating = false;
			GetDataFiles();
			return true;
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

		internal static void PlotReductionProfile(bool HighlightRePlot, string StarID, string ObserverID, string Date, bool ExcludeLargeResiduals, bool IncludeStarPath, bool IncludeAllPaths, bool ReReadHiresData)
		{
			Application.DoEvents();
			int width = ((Control)ObservedProfile.picProfile).get_Width();
			int height = ((Control)ObservedProfile.picProfile).get_Height();
			if (!(width < 100 || height < 50))
			{
				image = new Bitmap(width, height);
				Graphics graphics = Graphics.FromImage(image);
				DrawReductionProfile(graphics, width, height, Printer: false, AutoGenerate: false, HighlightRePlot, StarID, ObserverID, Date, ExcludeLargeResiduals, IncludeStarPath, IncludeAllPaths, ReReadHiresData);
				ObservedProfile.picProfile.set_Image((Image)image);
				graphics.Dispose();
				((Control)ObservedProfile).Focus();
			}
		}

		internal static void PrintReductionProfile()
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
				printDocument.PrintPage += PrintProfile_Reduction;
				printDocument.Print();
			}
		}

		internal static void PrintPreviewReductionProfile()
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
				printDocument.PrintPage += PrintProfile_Reduction;
				((Form)val).ShowDialog();
			}
		}

		internal static void PrintProfile_Reduction(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawReductionProfile(graphics, chartWidth, chartHeight, Printer: true, AutoGenerate: false, HighlightRePlot: false, "", "", "", ExcludeLargeResiduals: true, IncludeStarPath: false, AllPaths: false, ReReadHiresData: true);
		}

		internal static void DrawReductionProfile(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, bool AutoGenerate, bool HighlightRePlot, string StarID, string ObserverID, string Date, bool ExcludeLargeResiduals, bool IncludeStarPath, bool AllPaths, bool ReReadHiresData)
		{
			_ = new double[1810];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			float num4 = 0f;
			float num5 = 0f;
			float num6 = 0f;
			float num7 = 0f;
			float num8 = 0f;
			float num9 = 0f;
			float num10 = 0f;
			float num11 = 0f;
			int num12 = 0;
			int num13 = 0;
			string text = "";
			bool bWFlag = Settings.Default.BWFlag;
			bool flag = false;
			bool flag2 = false;
			bool flag3 = false;
			bool flag4 = true;
			bool flag5 = true;
			string text2 = "";
			string text3 = "";
			double num14 = 4.0;
			if (ExcludeLargeResiduals)
			{
				num14 = 0.2;
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			float num15 = (float)(AA_Max - AA_Min);
			if (num15 < 0f)
			{
				num15 += 360f;
			}
			Font font = new Font("Arial", 7f);
			Font font2 = new Font("Arial", 8f);
			Font font3 = new Font("Arial", 9f, FontStyle.Bold);
			Font font4 = new Font("Arial", 12f, FontStyle.Bold);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Pen pen4;
			Pen pen3;
			Pen pen2;
			Pen pen;
			Pen pen5;
			Pen pen6 = (pen5 = (pen4 = (pen3 = (pen2 = (pen = new Pen(Brushes.Black, 1f))))));
			Pen pen7;
			Pen pen8;
			Pen pen9;
			Pen pen10;
			Pen pen11;
			Pen pen12;
			Pen pen13;
			Pen pen14;
			Pen pen15;
			Pen pen16;
			Pen pen17;
			Pen pen18;
			Pen pen19;
			Pen pen20;
			Brush brush;
			Brush brush2;
			if (bWFlag || Printer || AutoGenerate)
			{
				pen7 = new Pen(Brushes.Black, 1f);
				pen8 = new Pen(Color.FromArgb(100, 255, 255, 255), 6f);
				new Pen(Brushes.Black, 2f);
				pen9 = new Pen(Brushes.Black, 1f);
				pen10 = new Pen(Brushes.Black, 2f);
				pen11 = new Pen(Brushes.Black, 1f);
				pen12 = new Pen(Brushes.Gray, 1f);
				if (Printer & EventsInColour)
				{
					pen13 = new Pen(Color.FromArgb(160, 150, 150, 100));
					pen14 = new Pen(Brushes.Red, 1f);
					pen15 = new Pen(Brushes.Lime, 1f);
					pen16 = new Pen(Brushes.Silver, 1f);
					pen17 = new Pen(Brushes.Red, 2f);
					pen6 = new Pen(Brushes.Lime, 2f);
					pen5 = new Pen(Brushes.Silver, 2f);
					pen4 = new Pen(Brushes.OrangeRed, 2f);
					pen3 = new Pen(Brushes.Green, 2f);
					new Pen(Brushes.LightSlateGray, 2f);
					pen2 = new Pen(Brushes.DarkOrange, 2f);
					pen = new Pen(Brushes.GreenYellow, 2f);
					new Pen(Brushes.LightSteelBlue, 2f);
					pen18 = new Pen(Brushes.Yellow, 2f);
					pen19 = new Pen(Brushes.Yellow, 2f);
					pen20 = new Pen(Brushes.Yellow, 2f);
				}
				else
				{
					pen13 = new Pen(Color.FromArgb(128, 0, 0, 0));
					pen14 = new Pen(Brushes.Black, 1f);
					pen15 = new Pen(Brushes.Black, 1f);
					pen16 = new Pen(Brushes.Black, 1f);
					pen18 = new Pen(Brushes.Black, 2f);
					pen19 = new Pen(Brushes.Black, 2f);
					pen20 = new Pen(Brushes.Black, 2f);
					pen17 = new Pen(Brushes.Black, 3f);
					pen6 = new Pen(Brushes.Black, 3f);
					pen5 = new Pen(Brushes.Black, 3f);
					pen4 = new Pen(Brushes.Black, 3f);
					pen3 = new Pen(Brushes.Black, 3f);
					new Pen(Brushes.Black, 3f);
					pen2 = new Pen(Brushes.Black, 2f);
					pen = new Pen(Brushes.Black, 2f);
					new Pen(Brushes.Black, 2f);
				}
				Pen pen21 = pen17;
				brush = Brushes.Black;
				brush2 = Brushes.Black;
			}
			else
			{
				pen7 = new Pen(Brushes.WhiteSmoke, 1f);
				pen8 = new Pen(Color.FromArgb(100, 0, 0, 0), 6f);
				new Pen(Brushes.WhiteSmoke, 2f);
				pen9 = new Pen(Brushes.Aqua, 1f);
				pen10 = new Pen(Brushes.Aquamarine, 1f);
				pen11 = new Pen(Brushes.Cyan, 1f);
				pen12 = new Pen(Brushes.LightBlue, 1f);
				if (!eventsInColour)
				{
					pen13 = new Pen(Color.FromArgb(128, 255, 255, 255));
					pen14 = new Pen(Brushes.White, 1f);
					pen15 = new Pen(Brushes.White, 1f);
					pen16 = new Pen(Brushes.White, 1f);
					pen18 = new Pen(Brushes.White, 2f);
					pen19 = new Pen(Brushes.White, 2f);
					pen20 = new Pen(Brushes.White, 2f);
					pen17 = new Pen(Brushes.White, 3f);
					pen6 = new Pen(Brushes.White, 3f);
					pen5 = new Pen(Brushes.White, 3f);
					pen4 = new Pen(Brushes.White, 2f);
					pen3 = new Pen(Brushes.White, 2f);
					new Pen(Brushes.White, 2f);
					pen2 = new Pen(Brushes.White, 2f);
					pen = new Pen(Brushes.White, 2f);
					new Pen(Brushes.White, 2f);
				}
				else
				{
					pen13 = new Pen(Color.FromArgb(160, 150, 150, 100));
					pen14 = new Pen(Brushes.Pink, 1f);
					pen15 = new Pen(Brushes.LightGreen, 1f);
					pen16 = new Pen(Brushes.Silver, 1f);
					pen17 = new Pen(Brushes.Red, 2f);
					pen6 = new Pen(Brushes.Lime, 2f);
					pen5 = new Pen(Brushes.Silver, 2f);
					pen4 = new Pen(Brushes.OrangeRed, 2f);
					pen3 = new Pen(Brushes.Green, 2f);
					new Pen(Brushes.LightSlateGray, 2f);
					pen2 = new Pen(Brushes.DarkOrange, 2f);
					pen = new Pen(Brushes.GreenYellow, 2f);
					new Pen(Brushes.LightSteelBlue, 2f);
					pen18 = new Pen(Brushes.Yellow, 2f);
					pen19 = new Pen(Brushes.Yellow, 2f);
					pen20 = new Pen(Brushes.Yellow, 2f);
				}
				brush = Brushes.White;
				brush2 = Brushes.Yellow;
			}
			pen10.DashPattern = new float[2] { 1f, 10f };
			pen11.DashPattern = new float[2] { 6f, 10f };
			pen12.DashPattern = new float[2] { 4f, 6f };
			Brush brush3;
			if (bWFlag || Printer || AutoGenerate)
			{
				formGraphics.Clear(Color.White);
				brush3 = Brushes.White;
			}
			else
			{
				formGraphics.Clear(Color.Black);
				brush3 = Brushes.Black;
			}
			MapCenterX = (float)ChartWidth / 2f;
			float num16 = (float)ChartHeight / 2f;
			float num17 = 0.95f * MapCenterX;
			float num18 = 0.05f * num16;
			float num19 = 1.95f * num16;
			MapLeft = MapCenterX - num17;
			MapRight = MapCenterX + num17;
			MapCenterY = (float)(HeightMax / (HeightMax - HeightMin) * 0.95 * (double)ChartHeight + (double)num18);
			VerticalScale_sec = (float)((double)(num19 - num18) / (HeightMax - HeightMin));
			HorizontalScale_deg = (float)((double)(2f * num17) / (AA_Max - AA_Min));
			float num20 = num17 / 50f;
			if (Use_LOLA_HiRes | ShowLOLAPoints)
			{
				if (AA_Min > AA_Max)
				{
					AA_Min -= 360.0;
				}
				LOLAHiRes.GetLimbData(ReductionProfile.l, b, AA_Min, AA_Max, MoonScale, KLimb);
				if (ShowLOLAPoints)
				{
					float num21 = 1f;
					for (int i = 0; i < KLimb.Count; i++)
					{
						num4 = (float)((double)MapRight - (double)HorizontalScale_deg * (KLimb[i].AA - AA_Min));
						num5 = (float)((double)MapCenterY - (double)VerticalScale_sec * KLimb[i].H);
						if (!(((num4 < MapLeft) | (num4 > MapRight)) || num5 < num18 || num5 > num19))
						{
							if (KLimb[i].Valid)
							{
								formGraphics.DrawEllipse(pen13, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
							}
							else
							{
								formGraphics.DrawEllipse(pen19, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
							}
						}
					}
				}
				if (use_LOLA_HiRes)
				{
					int num22 = 0;
					int NextLocation = 0;
					int NextLocation2 = 0;
					int NewStartLocation = 0;
					_ = AA_Min;
					while (!(KLimb[num22].AA > AA_Min))
					{
						num22++;
						if (num22 >= KLimb.Count)
						{
							break;
						}
					}
					LOLA_HiRes.GetNewStartingPoint(KLimb, num22, out NextLocation);
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
						num6 = (float)((double)MapRight - (double)HorizontalScale_deg * (KLimb[NextLocation].AA - AA_Min));
						num7 = (float)((double)MapCenterY - (double)VerticalScale_sec * KLimb[NextLocation].H);
						num8 = (float)((double)MapRight - (double)HorizontalScale_deg * (KLimb[NextLocation2].AA - AA_Min));
						num9 = (float)((double)MapCenterY - (double)VerticalScale_sec * KLimb[NextLocation2].H);
						formGraphics.DrawLine(pen9, num6, num7, num8, num9);
						NextLocation = NextLocation2;
					}
					while (KLimb[NextLocation2].AA < AA_Max);
				}
			}
			if (!Printer && !AutoGenerate && !HighlightRePlot)
			{
				ReductionPlot.Tags.Clear();
				GrazeList.Clear();
				ObserverList.Clear();
				num12 = 0;
				AllCount = 0;
			}
			if (HighlightRePlot & (ReductionPlot.Tags.Count > 0))
			{
				for (int j = 0; j < ReductionPlot.Tags.Count; j++)
				{
					if (ReductionPlot.Tags[j].CurrentEvent)
					{
						continue;
					}
					if ((ReductionPlot.Tags[j].StarID == StarID) | (ReductionPlot.Tags[j].ObserverID == ObserverID) | (ReductionPlot.Tags[j].Date == Date))
					{
						if (ReductionPlot.Tags[j].EventType % 3 == 0)
						{
							formGraphics.DrawEllipse(pen18, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
						}
						else if (ReductionPlot.Tags[j].EventType % 3 == 1)
						{
							formGraphics.DrawEllipse(pen19, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
						}
						else
						{
							formGraphics.DrawEllipse(pen20, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
						}
					}
					else if (ReductionPlot.Tags[j].EventType % 3 == 0)
					{
						if (ReductionPlot.Tags[j].EventType == 0)
						{
							formGraphics.DrawRectangle(pen14, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
							continue;
						}
						if (ReductionPlot.Tags[j].EventType == 3)
						{
							formGraphics.DrawEllipse(pen14, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
							continue;
						}
						formGraphics.DrawLine(pen14, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y, ReductionPlot.Tags[j].X + ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y);
						formGraphics.DrawLine(pen14, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y + ReductionPlot.Tags[j].CircleRadius);
					}
					else if (ReductionPlot.Tags[j].EventType % 3 == 1)
					{
						if (ReductionPlot.Tags[j].EventType == 1)
						{
							formGraphics.DrawRectangle(pen15, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
							continue;
						}
						if (ReductionPlot.Tags[j].EventType == 4)
						{
							formGraphics.DrawEllipse(pen15, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
							continue;
						}
						formGraphics.DrawLine(pen15, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y, ReductionPlot.Tags[j].X + ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y);
						formGraphics.DrawLine(pen15, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y + ReductionPlot.Tags[j].CircleRadius);
					}
					else if (ReductionPlot.Tags[j].EventType == 2)
					{
						formGraphics.DrawRectangle(pen16, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
					}
					else if (ReductionPlot.Tags[j].EventType == 5)
					{
						formGraphics.DrawEllipse(pen16, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius, 2f * ReductionPlot.Tags[j].CircleRadius);
					}
					else
					{
						formGraphics.DrawLine(pen16, ReductionPlot.Tags[j].X - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y, ReductionPlot.Tags[j].X + ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].Y);
						formGraphics.DrawLine(pen16, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y - ReductionPlot.Tags[j].CircleRadius, ReductionPlot.Tags[j].X, ReductionPlot.Tags[j].Y + ReductionPlot.Tags[j].CircleRadius);
					}
				}
			}
			else
			{
				LibrationRange = "";
				if (GrazeDataType > 0)
				{
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
					for (int k = 0; k < DataFiles.Count; k++)
					{
						StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + DataFiles[k]!.ToString());
						do
						{
							string text4 = streamReader.ReadLine();
							bool flag6 = false;
							if (text4.Length < 129)
							{
								continue;
							}
							string text5 = text4.Substring(60, 1);
							flag = text4.Substring(62, 1) == "G";
							bool flag7 = "GVP".Contains(text4.Substring(63, 1));
							if (!int.TryParse(text4.Substring(64, 1), out var result))
							{
								result = 0;
							}
							if ((((flag || flag7) | !limitToGrazes) && result <= 2) & (text5 != " "))
							{
								double result2;
								double result5;
								double result6;
								double result7;
								if (GrazeDataType > 3)
								{
									if (!double.TryParse(text4.Substring(98, 6), out result2))
									{
										result2 = 0.0;
									}
									result2 -= AA_Min;
									if (result2 < 0.0)
									{
										result2 += 360.0;
									}
									if (result2 > 180.0)
									{
										result2 -= 360.0;
									}
									if (result2 >= 0.0 && result2 < (double)num15)
									{
										if (!double.TryParse(text4.Substring(85, 6), out var result3))
										{
											result3 = 0.0;
										}
										if (!double.TryParse(text4.Substring(91, 6), out var result4))
										{
											result4 = 0.0;
										}
										if ((Math.Abs(ReductionProfile.l - result3) < num2) & (Math.Abs(b - result4) < num3))
										{
											if (!double.TryParse(text4.Substring(111, 6), out result5))
											{
												result5 = 1.0;
											}
											if (!double.TryParse(text4.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											if (!double.TryParse(text4.Substring(67, 8), out result7))
											{
												result7 = 0.0;
											}
											if (!double.TryParse(text4.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											result7 /= result5;
											if (Math.Abs(result7) < num14)
											{
												num4 = (float)((double)MapRight - (double)HorizontalScale_deg * result2);
												num5 = (float)((double)MapCenterY - (double)VerticalScale_sec * (result7 + result6));
												float num21 = (float)(4 - result) / 1.8f;
												if (num21 < 1f)
												{
													num21 = 1f;
												}
												if (text5 == "D")
												{
													if (flag)
													{
														formGraphics.DrawRectangle(pen14, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 0;
													}
													else if (flag7)
													{
														formGraphics.DrawEllipse(pen14, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 3;
													}
													else
													{
														formGraphics.DrawLine(pen14, num4 - num21, num5, num4 + num21, num5);
														formGraphics.DrawLine(pen14, num4, num5 + num21, num4, num5 - num21);
														num13 = 6;
													}
												}
												else if (text5 == "R")
												{
													if (flag)
													{
														formGraphics.DrawRectangle(pen15, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 1;
													}
													else if (flag7)
													{
														formGraphics.DrawEllipse(pen15, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 4;
													}
													else
													{
														formGraphics.DrawLine(pen15, num4 - num21, num5, num4 + num21, num5);
														formGraphics.DrawLine(pen15, num4, num5 + num21, num4, num5 - num21);
														num13 = 7;
													}
												}
												else if (flag)
												{
													formGraphics.DrawRectangle(pen16, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
													num13 = 2;
												}
												else if (flag7)
												{
													formGraphics.DrawEllipse(pen16, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
													num13 = 5;
												}
												else
												{
													formGraphics.DrawLine(pen16, num4 - num21, num5, num4 + num21, num5);
													formGraphics.DrawLine(pen16, num4, num5 + num21, num4, num5 - num21);
													num13 = 8;
												}
												AllCount++;
												if (!Printer && !AutoGenerate)
												{
													LunarGrazeTags lunarGrazeTags = new LunarGrazeTags();
													lunarGrazeTags.X = num4;
													lunarGrazeTags.Y = num5;
													lunarGrazeTags.CircleRadius = num21;
													lunarGrazeTags.EventType = num13;
													lunarGrazeTags.Date = text4.Substring(36, 11);
													lunarGrazeTags.ObserverID = text4.Substring(6, 20);
													lunarGrazeTags.StarID = text4.Substring(27, 8);
													if (!int.TryParse(text4.Substring(41, 2), out var result8))
													{
														result8 = 0;
													}
													lunarGrazeTags.Tag = text4.Substring(36, 5) + Utilities.ShortMonths[result8] + text4.Substring(43, 14) + ", " + text4.Substring(60, 2) + ", " + "  ok? ??".Substring(2 * result, 2) + "," + text4.Substring(5, 21).Trim() + ", " + text4.Substring(27, 1) + text4.Substring(28, 7).Trim();
													ReductionPlot.Tags.Add(lunarGrazeTags);
													flag6 = true;
												}
											}
										}
									}
								}
								else
								{
									if (!double.TryParse(text4.Substring(117, 7), out result2))
									{
										result2 = 0.0;
									}
									result2 -= PMin;
									if (result2 < -180.0)
									{
										result2 += 360.0;
									}
									if (result2 > 180.0)
									{
										result2 -= 360.0;
									}
									if (result2 >= 0.0 && result2 < (double)num15)
									{
										if (!double.TryParse(text4.Substring(124, 6), out var result9))
										{
											result9 = 0.0;
										}
										if (Math.Abs(result9 - D) < num)
										{
											if (!double.TryParse(text4.Substring(111, 6), out result5))
											{
												result5 = 1.0;
											}
											if (!double.TryParse(text4.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											if (!double.TryParse(text4.Substring(67, 8), out result7))
											{
												result7 = 0.0;
											}
											if (!double.TryParse(text4.Substring(104, 7), out result6))
											{
												result6 = 0.0;
											}
											result7 /= result5;
											if (Math.Abs(result7) < num14)
											{
												num4 = (float)((double)MapRight - (double)HorizontalScale_deg * result2);
												num5 = (float)((double)MapCenterY - (double)VerticalScale_sec * (result7 + result6));
												float num21 = (float)(4 - result) / 1.8f;
												if (num21 < 1f)
												{
													num21 = 1f;
												}
												if (text5 == "D")
												{
													if (flag)
													{
														formGraphics.DrawRectangle(pen14, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 0;
													}
													else if (flag7)
													{
														formGraphics.DrawEllipse(pen14, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 3;
													}
													else
													{
														formGraphics.DrawLine(pen14, num4 - num21, num5, num4 + num21, num5);
														formGraphics.DrawLine(pen14, num4, num5 + num21, num4, num5 - num21);
														num13 = 6;
													}
												}
												else if (text5 == "R")
												{
													if (flag)
													{
														formGraphics.DrawRectangle(pen15, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 1;
													}
													else if (flag7)
													{
														formGraphics.DrawEllipse(pen15, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
														num13 = 4;
													}
													else
													{
														formGraphics.DrawLine(pen15, num4 - num21, num5, num4 + num21, num5);
														formGraphics.DrawLine(pen15, num4, num5 + num21, num4, num5 - num21);
														num13 = 7;
													}
												}
												else if (flag)
												{
													formGraphics.DrawRectangle(pen16, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
													num13 = 2;
												}
												else if (flag7)
												{
													formGraphics.DrawEllipse(pen16, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
													num13 = 5;
												}
												else
												{
													formGraphics.DrawLine(pen16, num4 - num21, num5, num4 + num21, num5);
													formGraphics.DrawLine(pen16, num4, num5 + num21, num4, num5 - num21);
													num13 = 8;
												}
												AllCount++;
												if (!Printer && !AutoGenerate)
												{
													LunarGrazeTags lunarGrazeTags = new LunarGrazeTags();
													lunarGrazeTags.X = num4;
													lunarGrazeTags.Y = num5;
													lunarGrazeTags.CircleRadius = num21;
													lunarGrazeTags.EventType = num13;
													lunarGrazeTags.Date = text4.Substring(36, 11);
													lunarGrazeTags.ObserverID = text4.Substring(6, 20);
													lunarGrazeTags.StarID = text4.Substring(27, 8);
													if (!int.TryParse(text4.Substring(41, 2), out var result10))
													{
														result10 = 0;
													}
													lunarGrazeTags.Tag = text4.Substring(36, 5) + Utilities.ShortMonths[result10] + text4.Substring(43, 14) + ", " + text4.Substring(60, 2) + ", " + "  ok? ??".Substring(2 * result, 2) + "," + text4.Substring(5, 21).Trim() + ", " + text4.Substring(27, 1) + text4.Substring(28, 7).Trim();
													ReductionPlot.Tags.Add(lunarGrazeTags);
													flag6 = true;
												}
											}
										}
									}
								}
							}
							if (flag6 && flag)
							{
								string text6 = text4.Substring(36, 10) + text4.Substring(27, 8);
								if (text6 != text)
								{
									if (num12 > 0)
									{
										GrazeID.Count = num12;
										GrazeList.Add(GrazeID);
									}
									GrazeID = new LunarGrazeID();
									GrazeID.Date = text4.Substring(36, 10);
									GrazeID.StarID = text4.Substring(27, 8);
									GrazeID.L = text4.Substring(86, 4);
									GrazeID.B = text4.Substring(92, 4);
									text = text6;
									num12 = 1;
								}
								else
								{
									num12++;
								}
							}
							if (!flag6)
							{
								continue;
							}
							bool flag8 = false;
							string text7 = text4.Substring(6, 20);
							for (int l = 0; l < ObserverList.Count; l++)
							{
								if (ObserverList[l].Observer.ToString() == text7)
								{
									ObserverList[l].Count++;
									flag8 = true;
									break;
								}
							}
							if (!flag8)
							{
								ObserversName = new LunarObserver();
								ObserversName.Observer = text7;
								ObserversName.Count = 1;
								ObserverList.Add(ObserversName);
								ObserverList.Sort();
							}
						}
						while (!streamReader.EndOfStream);
						streamReader.Close();
					}
				}
			}
			formGraphics.DrawLine(pen8, MapLeft, MapCenterY, MapRight, MapCenterY);
			float num23;
			for (int m = (int)AA_Min; m <= (int)AA_Max; m++)
			{
				string text8 = $"{m:+0;-0}°";
				num23 = formGraphics.MeasureString(text8, font2).Width / 2f;
				num4 = (float)((double)MapRight - (double)HorizontalScale_deg * ((double)m - AA_Min));
				if ((num4 > MapLeft) & (num4 < MapRight))
				{
					formGraphics.DrawLine(pen8, num4, num18, num4, num18 + num20 + 3f);
					formGraphics.DrawLine(pen7, num4, num18, num4, num18 + num20);
					formGraphics.DrawLine(pen8, num4, MapCenterY - num20 - 3f, num4, MapCenterY + num20 + 3f);
					formGraphics.DrawLine(pen7, num4, MapCenterY - num20, num4, MapCenterY + num20);
					formGraphics.DrawLine(pen8, num4, num19, num4, num19 - num20 - 3f);
					formGraphics.DrawLine(pen7, num4, num19, num4, num19 - num20 - 3f);
					formGraphics.FillRectangle(brush3, num4 - num23, num19 - num20 - 10f, num23 * 2f, formGraphics.MeasureString(text8, font2).Height);
					formGraphics.DrawString(text8, font2, brush, num4 - num23, num19 - num20 - 10f);
				}
			}
			for (float num24 = 3.5f; num24 >= -3.5f; num24 -= 0.5f)
			{
				num5 = MapCenterY + VerticalScale_sec * num24;
				if (((num5 > num18) & (num5 < num19 - 2f * formGraphics.MeasureString("I", font2).Height)) && num24 != 0f)
				{
					string text8 = $"{0f - num24:+#.0;-#.0}\"";
					num23 = formGraphics.MeasureString(text8, font2).Width;
					formGraphics.DrawLine(pen8, MapLeft, num5, MapLeft + num20 + 3f, num5);
					formGraphics.DrawLine(pen7, MapLeft, num5, MapLeft + num20, num5);
					formGraphics.FillRectangle(brush3, MapLeft + num20 + 2f, num5 - 6f, num23, formGraphics.MeasureString(text8, font2).Height);
					formGraphics.DrawString(text8, font2, brush, MapLeft + num20 + 2f, num5 - 6f);
					formGraphics.DrawLine(pen8, MapRight, num5, MapRight - num20 - 3f, num5);
					formGraphics.DrawLine(pen7, MapRight, num5, MapRight - num20, num5);
					formGraphics.FillRectangle(brush3, MapRight - num23 - num20 - 2f, num5 - 6f, num23, formGraphics.MeasureString(text8, font2).Height);
					formGraphics.DrawString(text8, font2, brush, MapRight - num23 - num20 - 2f, num5 - 6f);
				}
			}
			formGraphics.DrawRectangle(pen7, MapLeft, num18, 2f * num17, num19 - num18);
			formGraphics.DrawLine(pen7, MapLeft, MapCenterY, MapRight, MapCenterY);
			text2 = (text3 = " ");
			flag3 = false;
			for (int n = 0; n < LunarObservations.Residuals.Count; n++)
			{
				if (LunarObservations.Residuals[n].WDSCode == " ")
				{
					continue;
				}
				if (text2 == " ")
				{
					text2 = "A";
				}
				if (!(LunarObservations.Residuals[n].WDSCode != text2))
				{
					continue;
				}
				text3 = LunarObservations.Residuals[n].WDSCode;
				flag3 = true;
				((Control)ObservedProfile.grpDouble).set_Enabled(true);
				LunarObservations.Get_DoubleStar_Sep_PA(XZ80Q.XZ, LunarObservations.Residuals[n].EventJDTime, LunarObservations.Residuals[n].WDSCode, out StarSepForPlot, out StarPAforPlot);
				if (!StarSepSet)
				{
					if (StarSepForPlot < 20.0)
					{
						ObservedProfile.updn_Sep.set_Value((decimal)StarSepForPlot);
					}
					ObservedProfile.updn_PA.set_Value((decimal)StarPAforPlot);
					((Control)ObservedProfile.lblDblSep).set_Text("Sepn " + string.Format("({0,1:f2})", StarSepForPlot));
					((Control)ObservedProfile.lblDblPA).set_Text("PA " + string.Format("({0,1:f2})", StarPAforPlot));
					Application.DoEvents();
				}
				break;
			}
			StarSepSet = true;
			((Control)ObservedProfile.grpDouble).set_Enabled(flag3);
			flag4 = true;
			flag5 = true;
			num10 = 0f;
			num11 = 0f;
			if (flag3)
			{
				flag4 = ObservedProfile.chkA.get_Checked();
				flag5 = ObservedProfile.chkB.get_Checked();
			}
			for (int num25 = 0; num25 < LunarObservations.Residuals.Count; num25++)
			{
				if (!(((SingleEvent_Number < 0) & (LunarObservations.Residuals[num25].GrazeFlag == "G")) | (num25 == SingleEvent_Number)) || (ExcludeLargeResiduals & (Math.Abs(LunarObservations.Residuals[num25].O_C) > 0.2)))
				{
					continue;
				}
				double result2 = LunarObservations.Residuals[num25].AA - AA_Min;
				if (result2 < 0.0)
				{
					result2 += 360.0;
				}
				if (result2 > 180.0)
				{
					result2 -= 360.0;
				}
				num4 = (float)((double)MapRight - (double)HorizontalScale_deg * result2);
				num5 = (float)((double)MapCenterY - (double)VerticalScale_sec * (LunarObservations.Residuals[num25].O_C + LunarObservations.Residuals[num25].Limb) / LunarObservations.Residuals[num25].Scale);
				float num21 = (float)(4 - LunarObservations.Residuals[num25].Certainty) / 1.8f;
				string text5 = LunarObservations.Residuals[num25].EventPhase;
				bool flag7 = "GVP".Contains(LunarObservations.Residuals[num25].MethodTimeRecording);
				if (num21 < 1f)
				{
					num21 = 1f;
				}
				flag2 = LunarObservations.Residuals[num25].Observer.Trim() == ObserverID;
				if (flag3)
				{
					if ((LunarObservations.Residuals[num25].WDSCode == text2 && !flag4) || (LunarObservations.Residuals[num25].WDSCode == text3 && !flag5))
					{
						continue;
					}
					if (LunarObservations.Residuals[num25].WDSCode == text3)
					{
						double num26 = (LunarObservations.Residuals[num25].AA - LunarObservations.Residuals[num25].PA) / (180.0 / Math.PI);
						_ = LunarObservations.Residuals[num25].PA / (180.0 / Math.PI);
						double num27 = (double)ObservedProfile.updn_Sep.get_Value() * Math.Sin((double)ObservedProfile.updn_PA.get_Value() / (180.0 / Math.PI)) - StarSepForPlot * Math.Sin(StarPAforPlot / (180.0 / Math.PI));
						double num28 = (double)ObservedProfile.updn_Sep.get_Value() * Math.Cos((double)ObservedProfile.updn_PA.get_Value() / (180.0 / Math.PI)) - StarSepForPlot * Math.Cos(StarPAforPlot / (180.0 / Math.PI));
						num10 = HorizontalScale_deg * (float)(num27 * Math.Cos(num26) + num28 * Math.Sin(num26)) / 16.276f / (float)LunarObservations.Residuals[num25].Scale;
						num11 = VerticalScale_sec * (float)(num27 * Math.Sin(num26) - num28 * Math.Cos(num26));
						num4 += num10;
						num5 += num11;
					}
				}
				if (text5 == "D")
				{
					Pen pen21 = pen17;
					if (flag3)
					{
						if (LunarObservations.Residuals[num25].WDSCode == text2)
						{
							pen21 = pen4;
						}
						else if (LunarObservations.Residuals[num25].WDSCode == text3)
						{
							pen21 = pen2;
						}
					}
					if (!eventsInColour)
					{
						if (!(bWFlag || Printer || AutoGenerate))
						{
							pen21 = new Pen(Brushes.White, 3f);
						}
					}
					else if (flag2)
					{
						pen21 = new Pen(Brushes.Yellow, 3f);
					}
					if (flag7)
					{
						formGraphics.DrawEllipse(pen21, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
						num13 = 3;
					}
					else
					{
						formGraphics.DrawLine(pen21, num4 - 2f * num21, num5, num4 + 2f * num21, num5);
						formGraphics.DrawLine(pen21, num4, num5 + 2f * num21, num4, num5 - 2f * num21);
						num13 = 6;
					}
				}
				else if (text5 == "R")
				{
					Pen pen21 = pen6;
					if (flag3)
					{
						if (LunarObservations.Residuals[num25].WDSCode == text2)
						{
							pen21 = pen3;
						}
						else if (LunarObservations.Residuals[num25].WDSCode == text3)
						{
							pen21 = pen;
						}
					}
					if (!eventsInColour)
					{
						if (!(bWFlag || Printer || AutoGenerate))
						{
							pen21 = new Pen(Brushes.White, 3f);
						}
					}
					else if (flag2)
					{
						pen21 = new Pen(Brushes.Yellow, 3f);
					}
					if (flag7)
					{
						formGraphics.DrawEllipse(pen21, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
						num13 = 4;
					}
					else
					{
						formGraphics.DrawLine(pen21, num4 - 2f * num21, num5, num4 + 2f * num21, num5);
						formGraphics.DrawLine(pen21, num4, num5 + 2f * num21, num4, num5 - 2f * num21);
						num13 = 7;
					}
				}
				else
				{
					pen5 = ((!eventsInColour) ? ((!(bWFlag || Printer || AutoGenerate)) ? new Pen(Brushes.White, 3f) : new Pen(Brushes.Black, 3f)) : ((!flag2) ? new Pen(Brushes.Silver, 2f) : new Pen(Brushes.Yellow, 3f)));
					if (flag7)
					{
						formGraphics.DrawEllipse(pen5, num4 - num21, num5 - num21, 2f * num21, 2f * num21);
						num13 = 5;
					}
					else
					{
						formGraphics.DrawLine(pen5, num4 - num21, num5, num4 + num21, num5);
						formGraphics.DrawLine(pen5, num4, num5 + num21, num4, num5 - num21);
						num13 = 8;
					}
				}
				if (!Printer && !AutoGenerate && !HighlightRePlot)
				{
					LunarGrazeTags lunarGrazeTags = new LunarGrazeTags();
					lunarGrazeTags.X = num4;
					lunarGrazeTags.Y = num5;
					lunarGrazeTags.CurrentEvent = true;
					lunarGrazeTags.CircleRadius = num21;
					lunarGrazeTags.EventType = num13;
					lunarGrazeTags.Date = LunarObservations.Residuals[num25].Date;
					lunarGrazeTags.ObserverID = LunarObservations.Residuals[num25].Observer.Trim();
					lunarGrazeTags.StarID = LunarObservations.Residuals[num25].StarCat_Number.Trim() + LunarObservations.Residuals[num25].WDSCode.Trim();
					lunarGrazeTags.Tag = LunarObservations.Residuals[num25].Date + LunarObservations.Residuals[num25].EventTime + ", " + LunarObservations.Residuals[num25].EventPhase + LunarObservations.Residuals[num25].EventLimb + ", " + "  ok? ??............".Substring(2 * LunarObservations.Residuals[num25].Certainty, 2) + "," + lunarGrazeTags.ObserverID.Trim() + ", " + lunarGrazeTags.StarID.Trim();
					ReductionPlot.Tags.Add(lunarGrazeTags);
					bool flag6 = true;
				}
			}
			if (num12 > 0 && !HighlightRePlot)
			{
				GrazeID.Count = num12;
				GrazeList.Add(GrazeID);
			}
			if (!Printer && !AutoGenerate && !HighlightRePlot)
			{
				ReductionPlot.Tags.Sort();
				LunarGrazeID.SortField = 4;
				GrazeList.Sort();
			}
			if (ShowSlopeLines)
			{
				formGraphics.DrawLine(pen10, MapCenterX - 0.1228f * HorizontalScale_deg, num18, MapCenterX - 0.1228f * HorizontalScale_deg, num19);
				formGraphics.DrawLine(pen10, MapCenterX, num18, MapCenterX, num19);
				formGraphics.DrawLine(pen10, MapCenterX + 0.1228f * HorizontalScale_deg, num18, MapCenterX + 0.1228f * HorizontalScale_deg, num19);
			}
			if (IncludeStarPath)
			{
				bool flag9 = false;
				string[] array = new string[5] { "", "", "", "", "" };
				int[] array2 = new int[5];
				string[] array3 = new string[5] { "", "", "", "", "" };
				int num29 = 1;
				for (int num30 = 0; num30 < LunarObservations.OccMain.Observers.Count; num30++)
				{
					if (ObserverID.Trim() == LunarObservations.OccMain.Observers[num30].ObserverName || AllPaths)
					{
						string observerCodeForEvent = LunarObservations.OccMain.Observers[num30].ObserverCodeForEvent;
						for (int num31 = 0; num31 < LunarObservations.OccMain.Events.Count; num31++)
						{
							if (LunarObservations.OccMain.Events[num31].EventObserver == observerCodeForEvent || AllPaths)
							{
								string eventTelescope = LunarObservations.OccMain.Events[num31].EventTelescope;
								for (int num32 = 0; num32 < LunarObservations.OccMain.Telescopes.Count; num32++)
								{
									if (!(LunarObservations.OccMain.Telescopes[num32].TelescopeCodeForEvent == eventTelescope || AllPaths))
									{
										continue;
									}
									flag9 = true;
									double longitude = LunarObservations.OccMain.Telescopes[num32].Longitude;
									double latitude = LunarObservations.OccMain.Telescopes[num32].Latitude;
									double altitude = LunarObservations.OccMain.Telescopes[num32].Altitude;
									bool height_is_MSL = "M ".Contains(LunarObservations.OccMain.Telescopes[num32].VerticalDatum);
									int datumNumber = LunarObservations.OccMain.Telescopes[num32].DatumNumber;
									array[0] = LunarObservations.OccMain.Events[num31].StarCat;
									array2[0] = LunarObservations.OccMain.Events[num31].StarNumber;
									array3[0] = LunarObservations.OccMain.Events[num31].WDS;
									num29 = 1;
									double eventJDTime;
									double num33 = (eventJDTime = LunarObservations.OccMain.Events[num31].EventJDTime);
									for (int num34 = 0; num34 < LunarObservations.OccMain.Events.Count; num34++)
									{
										if (!(LunarObservations.OccMain.Events[num34].EventTelescope == eventTelescope))
										{
											continue;
										}
										if (num33 > LunarObservations.OccMain.Events[num34].EventJDTime)
										{
											num33 = LunarObservations.OccMain.Events[num34].EventJDTime;
										}
										if (eventJDTime < LunarObservations.OccMain.Events[num34].EventJDTime)
										{
											eventJDTime = LunarObservations.OccMain.Events[num34].EventJDTime;
										}
										if (num29 >= 5)
										{
											continue;
										}
										bool flag10 = false;
										for (int num35 = 0; num35 < num29; num35++)
										{
											if (LunarObservations.OccMain.Events[num34].WDS == array3[num35])
											{
												flag10 = true;
											}
										}
										if (!flag10)
										{
											array[num29] = LunarObservations.OccMain.Events[num34].StarCat;
											array2[num29] = LunarObservations.OccMain.Events[num34].StarNumber;
											array3[num29] = LunarObservations.OccMain.Events[num34].WDS;
											num29++;
										}
									}
									num33 -= 0.003;
									eventJDTime += 0.003;
									string Reduction = "";
									for (int num36 = 0; num36 < num29; num36++)
									{
										ArrayList arrayList = new ArrayList();
										for (double num37 = num33; num37 <= eventJDTime; num37 += 0.0001)
										{
											LunarObservations.ReduceAnObservation(num37, longitude, latitude, altitude, height_is_MSL, datumNumber, array[num36], array2[num36], UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, array3[num36], ApplyLimbCorrn: false, VizierReduction: false, out Reduction, out var Residual, out var _, out var _, out var _, out var _, out var _, out var _);
											if (!double.TryParse(Reduction.Substring(30, 6), out var result11))
											{
												result11 = 0.0;
											}
											if (!double.TryParse(Reduction.Substring(43, 5), out var result12))
											{
												result12 = 1.0;
											}
											double num38;
											for (num38 = result11 - AA_Min; num38 > 180.0; num38 -= 360.0)
											{
											}
											for (; num38 < -180.0; num38 += 360.0)
											{
											}
											num4 = (float)((double)MapRight - (double)HorizontalScale_deg * num38);
											num5 = (float)((double)MapCenterY - (double)VerticalScale_sec * Residual / result12);
											arrayList.Add(new PointF(num4, num5));
										}
										PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
										if (num36 < 2)
										{
											formGraphics.DrawLines(pen11, points);
										}
										else
										{
											formGraphics.DrawLines(pen12, points);
										}
									}
								}
							}
							if (flag9)
							{
								break;
							}
						}
					}
					if (flag9)
					{
						break;
					}
				}
			}
			num23 = formGraphics.MeasureString(MapTitle, font4).Width / 2f;
			formGraphics.FillRectangle(brush3, MapCenterX - num23, num18 + 8f, num23 * 2f, formGraphics.MeasureString(MapTitle, font4).Height);
			formGraphics.DrawString(MapTitle, font4, brush, MapCenterX - num23, num18 + 8f);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush, MapLeft + 3f, num18 + 3f);
			if (ObserverID.Length > 0)
			{
				formGraphics.DrawString(ObserverID, font3, brush2, MapLeft + 3f, num18 + 14f);
			}
			else if (Date.Length > 0)
			{
				formGraphics.DrawString(Date, font3, brush2, MapLeft + 3f, num18 + 14f);
			}
			else if (StarID.Length > 0)
			{
				formGraphics.DrawString(StarID, font3, brush2, MapLeft + 3f, num18 + 14f);
			}
			if (LibrationRange.Length > 0)
			{
				num4 = MapCenterX - formGraphics.MeasureString(LibrationRange, font2).Width / 2f;
				num5 = num18 + 24f;
				formGraphics.DrawString(LibrationRange, font2, brush, num4, num5);
			}
			if (ShowSlopeLines)
			{
				num4 = MapCenterX - formGraphics.MeasureString("< +/- 2\" >", font2).Width / 2f;
				num5 = num18 + 60f;
				formGraphics.DrawString("< +/- 2\" >", font2, brush, num4, num5);
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		internal static string ToolTipText(float x, float y)
		{
			CurrentAAofMouse = (double)((2f * MapCenterX - MapLeft - x) / HorizontalScale_deg) + AA_Min;
			double num = (MapCenterY - y) / VerticalScale_sec;
			return "AA = " + string.Format("{0,1:F2}", CurrentAAofMouse) + ",  Height = " + string.Format("{0,1:F2}", num);
		}

		internal static string ToolTipTextProfileSlope(float x, float y)
		{
			double x2 = (double)((0f - x) / HorizontalScale_deg) / (180.0 / Math.PI) * 932.58;
			double num = Math.Atan2(y / VerticalScale_sec, x2) * (180.0 / Math.PI);
			if (num > 90.0)
			{
				num = -180.0 + num;
			}
			if (num < -90.0)
			{
				num = 180.0 + num;
			}
			return "Limb slope = " + string.Format("{0,1:F3} deg.", num);
		}
	}
}
