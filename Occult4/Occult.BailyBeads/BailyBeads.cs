using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using GifCreator;
using Occult.Mapping;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class BailyBeads
	{
		private const double Radian = 180.0 / Math.PI;

		internal static bool CancelMovie = false;

		internal static int GrazeDataType = 0;

		internal static bool LimitToGrazes = false;

		internal static bool MoviePause = false;

		internal static bool ResumeFlag = false;

		internal static bool ResumeLess10 = false;

		private static double PauseTime;

		private static string GIF_Location = "";

		private static string GIF_PlotTime = "";

		private static string GIF_FileDate = "";

		internal static float Image_CenterX;

		internal static float Image_CenterY;

		internal static float Image_RadialScale;

		internal static float MapRight;

		internal static float MapLeft;

		internal static float HorizontalScale_deg;

		internal static float MapCenterY;

		internal static float Map_VerticalScale_sec;

		internal static int AA_Min;

		internal static double SunMoonX;

		internal static double SunMoonY;

		internal static double MoonR;

		internal static double SunR;

		internal static double MoonScale;

		internal static double L;

		internal static double B;

		internal static double C;

		internal static double Image_Scale;

		internal static double CentralAxisAngle;

		internal static bool Mirror;

		internal static bool Printer = false;

		internal static BailyBeadsMain BailyBeads_Main;

		internal static EclipseImage Eclipse_Image;

		internal static LimbPlot Limb_Plot;

		internal static Results Results;

		internal static LimbHeights Limb_Heights;

		internal static List<BailyResults> Observations = new List<BailyResults>();

		internal static BailyResults ResultLine;

		private static ArrayList DataFiles = new ArrayList();

		internal static List<Points> KLimb = new List<Points>();

		public static void Show_All()
		{
			Show_Main();
			Show_Eclipse_Image();
			Show_Limb_Plot();
			Show_Results();
			((Control)BailyBeads_Main).Focus();
		}

		internal static void Show_Main()
		{
			try
			{
				((Control)BailyBeads_Main).Show();
			}
			catch
			{
				BailyBeads_Main = new BailyBeadsMain();
				((Control)BailyBeads_Main).Show();
			}
		}

		internal static void Show_Eclipse_Image()
		{
			try
			{
				((Control)Eclipse_Image).Show();
			}
			catch
			{
				Eclipse_Image = new EclipseImage();
				((Control)Eclipse_Image).Show();
			}
		}

		internal static void Show_Limb_Plot()
		{
			try
			{
				((Control)Limb_Plot).Show();
			}
			catch
			{
				Limb_Plot = new LimbPlot();
				((Control)Limb_Plot).Show();
			}
		}

		internal static void Show_Results()
		{
			try
			{
				((Control)Results).Show();
			}
			catch
			{
				Results = new Results();
				((Control)Results).Show();
			}
			Results.UpdateBox();
		}

		internal static void Show_Heights()
		{
			try
			{
				((Control)Limb_Heights).Show();
			}
			catch
			{
				Limb_Heights = new LimbHeights();
				((Control)Limb_Heights).Show();
			}
		}

		internal static void CloseResults()
		{
			try
			{
				((Form)Results).Close();
				((Component)(object)Results).Dispose();
			}
			catch
			{
			}
		}

		internal static void Close_Limb_Plot()
		{
			try
			{
				((Form)Limb_Plot).Close();
				((Component)(object)Limb_Plot).Dispose();
			}
			catch
			{
			}
		}

		internal static void Close_Eclipse_Image()
		{
			try
			{
				((Form)Eclipse_Image).Close();
				((Component)(object)Eclipse_Image).Dispose();
			}
			catch
			{
			}
		}

		internal static void Close_Heights()
		{
			try
			{
				((Form)Limb_Heights).Close();
				((Component)(object)Limb_Heights).Dispose();
			}
			catch
			{
			}
		}

		internal static void GetDataFiles()
		{
			DataFiles.Clear();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\", "RArchive*.*");
			foreach (string path in files)
			{
				DataFiles.Add(Path.GetFileName(path));
			}
		}

		internal static void ReadObservations(StreamReader ReadFile)
		{
			Observations.Clear();
			while (!ReadFile.EndOfStream)
			{
				ResultLine = new BailyResults();
				ResultLine.ReadLine(ReadFile.ReadLine()!.PadRight(28));
				Observations.Add(ResultLine);
			}
			Results.UpdateBox();
			Solve();
		}

		internal static void WriteObservations(StreamWriter WriteFile)
		{
			for (int i = 0; i < ((ObjectCollection)Results.boxEvents.get_Items()).get_Count(); i++)
			{
				WriteFile.WriteLine(Observations[i].ToString());
			}
		}

		internal static void TransferObservationToPlotTime(int Line)
		{
			((Control)BailyBeads_Main.txtHr).set_Text(Observations[Line].Hr.ToString());
			((Control)BailyBeads_Main.txtMin).set_Text(Observations[Line].Min.ToString());
			BailyBeads_Main.updnSec.set_Value((decimal)Observations[Line].Sec);
			BailyBeads_Main.updnAxisAngle.set_Value((decimal)Observations[Line].AA);
			BailyBeads_Main.Plot();
		}

		internal static void PlotImages(double JDatStart, double JDatEnd, double TimeStep, double Longitude, double Latitude, double Altitude, string Datum, double CentralAA, double PlotScale, double RAOffset, double DecOffset, double SunRadiusCorrn, bool PlotMirror, bool Movie, bool AnimatedGIF)
		{
			List<string> list = new List<string>();
			int num = 10000;
			GIF_Location = Utilities.DEGtoDMS(Longitude * (180.0 / Math.PI), 4, 0, MinutesOnly: false) + ",  " + Utilities.DEGtoDMS(Latitude * (180.0 / Math.PI), 3, 0, MinutesOnly: false) + ",  " + string.Format("{0,1:F0}m", Altitude);
			GIF_FileDate = Utilities.Date_from_JD(JDatStart, 0);
			Image_Scale = PlotScale;
			Mirror = PlotMirror;
			CentralAxisAngle = CentralAA;
			if (!Movie)
			{
				JDatEnd = JDatStart;
				TimeStep = 1.0;
			}
			if (AnimatedGIF)
			{
				string path = Utilities.AppPath + "\\Predictions\\AnimatedGIF";
				if (!Directory.Exists(path))
				{
					Directory.CreateDirectory(path);
				}
				string[] files = Directory.GetFiles(path, "*.gif");
				foreach (string path2 in files)
				{
					try
					{
						File.Delete(path2);
					}
					catch
					{
					}
				}
			}
			Utilities.EarthOrientationParameters(JDatStart, out var x, out var y, out var _);
			Longitude += (x * Math.Cos(Longitude) - y * Math.Sin(Longitude)) / 3600.0 / (180.0 / Math.PI);
			Latitude += (x * Math.Sin(Longitude) + y * Math.Cos(Longitude)) * Math.Tan(Latitude) / 3600.0 / (180.0 / Math.PI);
			for (double num2 = JDatStart; num2 <= JDatEnd; num2 += TimeStep)
			{
				GIF_PlotTime = Utilities.DEGtoDMS(24.0 * ((num2 - 0.5) % 1.0), 2, 1, MinutesOnly: false);
				((Control)BailyBeads_Main.cmdCancel).set_Text(GIF_PlotTime.Substring(3));
				Application.DoEvents();
				if (CancelMovie)
				{
					return;
				}
				if (MoviePause)
				{
					PauseTime = num2;
					MoviePause = false;
					break;
				}
				if (ResumeFlag)
				{
					num2 = PauseTime;
					ResumeFlag = false;
				}
				if (ResumeLess10)
				{
					num2 = PauseTime - 10.0 * TimeStep;
					ResumeLess10 = false;
				}
				Utilities.TopocentricMoon(num2, Longitude, Latitude, Altitude, Altitude_is_MSL: true, 0, out var RA, out var Dec, out var MoonRadius_Radians, out MoonScale, out L, out B, out C, out var _);
				Utilities.TopocentricSun(num2, Longitude, Latitude, Altitude, out var raPlanet, out var decPlanet, out var PlanetRadius, out var _);
				SunMoonX = (raPlanet - RA) * (180.0 / Math.PI) * 3600.0 * Math.Cos((Dec + decPlanet) / 2.0) + RAOffset;
				SunMoonY = (decPlanet - Dec) * (180.0 / Math.PI) * 3600.0 + DecOffset;
				PlanetRadius *= 1.0 + SunRadiusCorrn / 959.63;
				((Control)BailyBeads_Main.txt_x).set_Text(string.Format("{0,3:F2}", SunMoonX));
				((Control)BailyBeads_Main.txt_y).set_Text(string.Format("{0,3:F2}", SunMoonY));
				((Control)BailyBeads_Main.txtL).set_Text(string.Format("{0,3:F2}", L));
				((Control)BailyBeads_Main.txtB).set_Text(string.Format("{0,3:F2}", B));
				((Control)BailyBeads_Main.txtC).set_Text(string.Format("{0,3:F2}", C));
				MoonR = MoonRadius_Radians * (180.0 / Math.PI) * 3600.0;
				SunR = PlanetRadius * (180.0 / Math.PI) * 3600.0;
				if (BailyBeads_Main.LunarLimbToolStripMenuItem.get_Checked())
				{
					DrawLunarLimb();
				}
				if (!BailyBeads_Main.eclipseImageToolStripMenuItem.get_Checked())
				{
					continue;
				}
				DrawEclipseImage();
				if (AnimatedGIF)
				{
					string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num + ".gif";
					try
					{
						Eclipse_Image.picEclipse.get_Image().Save(text, ImageFormat.Gif);
						list.Add(text);
					}
					catch
					{
					}
					num++;
				}
			}
			if (BailyBeads_Main.eclipseImageToolStripMenuItem.get_Checked() && AnimatedGIF)
			{
				global::GifCreator.GifCreator.Create_Animated_Gif(list, "Sun_" + GIF_FileDate.Replace(" ", "_") + ".gif");
			}
		}

		internal static void DrawLunarLimb()
		{
			if (Limb_Plot == null || Limb_Plot.Updating)
			{
				return;
			}
			Limb_Plot.Updating = true;
			int width = ((Control)Limb_Plot.picLimb).get_Width();
			int height = ((Control)Limb_Plot.picLimb).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			_ = new double[1000];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			float num4 = 0f;
			float num5 = 0f;
			_ = Settings.Default.BWFlag;
			bool flag = false;
			int chartWidthDeg = Limb_Plot.ChartWidthDeg;
			float num6 = Limb_Plot.ChartHeightSec;
			float num7 = -Limb_Plot.ChartHeightSec;
			bool @checked = Limb_Plot.chkLOLApoints.get_Checked();
			new Font("Arial", 7f);
			Font font = new Font("Arial", 8f);
			new Font("Arial", 12f, FontStyle.Bold);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Pen pen = new Pen(Brushes.Black, 1f);
			new Pen(Brushes.Black, 2f);
			Pen pen2 = new Pen(Brushes.Black, 2f);
			Pen pen3 = new Pen(Brushes.SaddleBrown, 1f);
			Pen pen4 = new Pen(Brushes.Black, 1f);
			Pen pen5 = new Pen(Brushes.Black, 1f);
			Pen pen6 = new Pen(Brushes.Black, 1f);
			new Pen(Brushes.Black, 2f);
			new Pen(Brushes.Black, 2f);
			new Pen(Brushes.Black, 2f);
			new Pen(Brushes.Black, 3f);
			new Pen(Brushes.Black, 3f);
			new Pen(Brushes.Black, 3f);
			Brush black = Brushes.Black;
			_ = Brushes.White;
			graphics.Clear(Color.White);
			float num8 = (float)width / 2f;
			float num9 = (float)height / 2f;
			float num10 = 0.95f * num8;
			float num11 = 0.05f * num9;
			float num12 = 1.95f * num9;
			MapLeft = num8 - num10;
			MapRight = num8 + num10;
			MapCenterY = (float)((double)(num6 / (num6 - num7)) * 0.95 * (double)height + (double)num11);
			Map_VerticalScale_sec = (num12 - num11) / (num6 - num7);
			HorizontalScale_deg = 2f * num10 / (float)chartWidthDeg;
			AA_Min = (int)(CentralAxisAngle - (double)((float)chartWidthDeg / 2f));
			if (AA_Min < 0)
			{
				AA_Min += 360;
			}
			int num13 = AA_Min + chartWidthDeg;
			float num14 = num10 / 50f;
			if (GrazeDataType > 0)
			{
				switch (GrazeDataType)
				{
				case 1:
					num = 0.3;
					break;
				case 2:
					num = 0.5;
					break;
				case 3:
					num = 1.0;
					break;
				case 4:
					num2 = 1.0;
					num3 = 0.5;
					break;
				case 5:
					num2 = 2.0;
					num3 = 0.5;
					break;
				case 6:
					num2 = 1.0;
					num3 = 1.0;
					break;
				case 7:
					num2 = 2.0;
					num3 = 1.0;
					break;
				case 8:
					num2 = 4.0;
					num3 = 1.0;
					break;
				case 9:
					num2 = 4.0;
					num3 = 2.0;
					break;
				}
				for (int i = 0; i < DataFiles.Count; i++)
				{
					StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\" + DataFiles[i]!.ToString());
					do
					{
						string text = streamReader.ReadLine();
						if (text.Length < 129)
						{
							continue;
						}
						string text2 = text.Substring(60, 1);
						flag = text.Substring(62, 1) == "G";
						bool flag2 = "GVP".Contains(text.Substring(63, 1));
						if (!int.TryParse(text.Substring(64, 1), out var result))
						{
							result = 0;
						}
						if (!((((flag || flag2) | !LimitToGrazes) && result <= 2) & (text2 != " ")))
						{
							continue;
						}
						double result2;
						double result5;
						double result6;
						float num15;
						if (GrazeDataType > 3)
						{
							if (!double.TryParse(text.Substring(98, 6), out result2))
							{
								result2 = 0.0;
							}
							result2 -= (double)AA_Min;
							if (result2 < 0.0)
							{
								result2 += 360.0;
							}
							if (result2 > 180.0)
							{
								result2 -= 360.0;
							}
							if (!(Math.Abs(result2) < (double)chartWidthDeg))
							{
								continue;
							}
							if (!double.TryParse(text.Substring(85, 6), out var result3))
							{
								result3 = 0.0;
							}
							if (!double.TryParse(text.Substring(91, 6), out var result4))
							{
								result4 = 0.0;
							}
							if (!((Math.Abs(L - result3) < num2) & (Math.Abs(B - result4) < num3)))
							{
								continue;
							}
							if (!double.TryParse(text.Substring(111, 6), out result5))
							{
								result5 = 1.0;
							}
							if (!double.TryParse(text.Substring(68, 8), out result6))
							{
								result6 = 0.0;
							}
							result6 /= result5;
							if (!(Math.Abs(result6) < 4.0))
							{
								continue;
							}
							num4 = (float)((double)MapRight - (double)HorizontalScale_deg * result2);
							num5 = (float)((double)MapCenterY - (double)Map_VerticalScale_sec * result6);
							num15 = (float)(4 - result) / 1.8f;
							if (num15 < 1f)
							{
								num15 = 1f;
							}
							if (text2 == "D")
							{
								if (flag)
								{
									graphics.DrawRectangle(pen4, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
									continue;
								}
								if (flag2)
								{
									graphics.DrawEllipse(pen4, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
									continue;
								}
								graphics.DrawLine(pen4, num4 - num15, num5, num4 + num15, num5);
								graphics.DrawLine(pen4, num4, num5 + num15, num4, num5 - num15);
							}
							else if (text2 == "R")
							{
								if (flag)
								{
									graphics.DrawRectangle(pen5, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
									continue;
								}
								if (flag2)
								{
									graphics.DrawEllipse(pen5, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
									continue;
								}
								graphics.DrawLine(pen5, num4 - num15, num5, num4 + num15, num5);
								graphics.DrawLine(pen5, num4, num5 + num15, num4, num5 - num15);
							}
							else if (flag)
							{
								graphics.DrawRectangle(pen6, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
							}
							else if (flag2)
							{
								graphics.DrawEllipse(pen6, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
							}
							else
							{
								graphics.DrawLine(pen6, num4 - num15, num5, num4 + num15, num5);
								graphics.DrawLine(pen6, num4, num5 + num15, num4, num5 - num15);
							}
							continue;
						}
						if (!double.TryParse(text.Substring(117, 7), out result2))
						{
							result2 = 0.0;
						}
						for (result2 -= (double)AA_Min; result2 < -180.0; result2 += 360.0)
						{
						}
						while (result2 > 180.0)
						{
							result2 -= 360.0;
						}
						if (!(Math.Abs(result2) < (double)chartWidthDeg))
						{
							continue;
						}
						if (!double.TryParse(text.Substring(124, 6), out var result7))
						{
							result7 = 0.0;
						}
						if (!(Math.Abs(result7) < num))
						{
							continue;
						}
						if (!double.TryParse(text.Substring(111, 6), out result5))
						{
							result5 = 1.0;
						}
						if (!double.TryParse(text.Substring(68, 8), out result6))
						{
							result6 = 0.0;
						}
						result6 /= result5;
						if (!(Math.Abs(result6) < 4.0))
						{
							continue;
						}
						num4 = (float)((double)MapRight - (double)HorizontalScale_deg * result2);
						num5 = (float)((double)MapCenterY - (double)Map_VerticalScale_sec * result6);
						num15 = (float)(4 - result) / 1.8f;
						if (num15 < 1f)
						{
							num15 = 1f;
						}
						if (text2 == "D")
						{
							if (flag)
							{
								graphics.DrawRectangle(pen4, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
								continue;
							}
							if (flag2)
							{
								graphics.DrawEllipse(pen4, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
								continue;
							}
							graphics.DrawLine(pen4, num4 - num15, num5, num4 + num15, num5);
							graphics.DrawLine(pen4, num4, num5 + num15, num4, num5 - num15);
						}
						else if (text2 == "R")
						{
							if (flag)
							{
								graphics.DrawRectangle(pen5, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
								continue;
							}
							if (flag2)
							{
								graphics.DrawEllipse(pen5, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
								continue;
							}
							graphics.DrawLine(pen5, num4 - num15, num5, num4 + num15, num5);
							graphics.DrawLine(pen5, num4, num5 + num15, num4, num5 - num15);
						}
						else if (flag)
						{
							graphics.DrawRectangle(pen6, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
						}
						else if (flag2)
						{
							graphics.DrawEllipse(pen6, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
						}
						else
						{
							graphics.DrawLine(pen6, num4 - num15, num5, num4 + num15, num5);
							graphics.DrawLine(pen6, num4, num5 + num15, num4, num5 - num15);
						}
					}
					while (!streamReader.EndOfStream);
					streamReader.Close();
				}
			}
			ArrayList arrayList = new ArrayList();
			if (@checked & (MoonScale > 0.0))
			{
				if (@checked)
				{
					LOLAHiRes.GetLimbData(L, B, AA_Min, num13, MoonScale, KLimb);
				}
				float num15 = 1f;
				for (int j = 0; j < KLimb.Count; j++)
				{
					double num16 = KLimb[j].AA - (double)AA_Min;
					if (num16 < 0.0)
					{
						num16 += 360.0;
					}
					num4 = (float)((double)MapRight - (double)HorizontalScale_deg * num16);
					if (Mirror)
					{
						num4 += 2f * (num8 - num4);
					}
					num5 = (float)((double)MapCenterY - (double)Map_VerticalScale_sec * KLimb[j].H * MoonScale);
					if (!(((num4 < MapLeft) | (num4 > MapRight)) || num5 < num11 || num5 > num12))
					{
						graphics.DrawEllipse(pen3, num4 - num15, num5 - num15, 2f * num15, 2f * num15);
					}
				}
			}
			float num17 = 0.1f;
			if (chartWidthDeg > 20)
			{
				num17 = 0.2f;
			}
			if (chartWidthDeg > 40)
			{
				num17 = 0.5f;
			}
			double num18 = Math.Atan2(SunMoonX, SunMoonY);
			double num19 = Math.Sqrt(SunMoonX * SunMoonX + SunMoonY * SunMoonY);
			arrayList.Clear();
			for (float num20 = 0f; num20 <= (float)chartWidthDeg; num20 += num17)
			{
				double num21 = ((double)((float)AA_Min + num20) + C) / (180.0 / Math.PI);
				double num22 = ((!(SunR + MoonR - num19 > 0.0)) ? 200.0 : (num19 * Math.Cos(num21 - num18) + Math.Sqrt(SunR * SunR - num19 * num19 * Math.Sin(num21 - num18) * Math.Sin(num21 - num18)) - MoonR));
				num4 = MapRight - HorizontalScale_deg * num20;
				if (Mirror)
				{
					num4 += 2f * (num8 - num4);
				}
				num5 = (float)((double)MapCenterY - (double)Map_VerticalScale_sec * num22);
				arrayList.Add(new PointF(num4, num5));
			}
			if (arrayList.Count > 1)
			{
				try
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					graphics.DrawLines(pen2, points);
				}
				catch
				{
				}
			}
			graphics.DrawRectangle(pen, MapLeft, num11, 2f * num10, num12 - num11);
			graphics.DrawLine(pen, MapLeft, MapCenterY, MapRight, MapCenterY);
			for (int k = 0; k <= chartWidthDeg; k++)
			{
				int num23 = AA_Min + k;
				if (num23 >= 360)
				{
					num23 -= 360;
				}
				string text3 = $"{num23:+0;-0}°";
				float num24 = graphics.MeasureString(text3, font).Width / 2f;
				num4 = MapRight - HorizontalScale_deg * (float)k;
				if (Mirror)
				{
					num4 += 2f * (num8 - num4);
				}
				if ((num4 > MapLeft) & (num4 < MapRight) & (((num23 % 10 == 0) | (num23 % 5 == 0 && chartWidthDeg < 100)) || chartWidthDeg < 40))
				{
					graphics.DrawLine(pen, num4, num11, num4, num11 + num14);
					graphics.DrawLine(pen, num4, MapCenterY - num14, num4, MapCenterY + num14);
					graphics.DrawLine(pen, num4, num12, num4, num12 - num14);
					graphics.DrawString(text3, font, black, num4 - num24, num11 + num14 + 2f);
					graphics.DrawString(text3, font, black, num4 - num24, num12 - num14 - 10f);
				}
			}
			for (float num25 = 10f; num25 >= -10f; num25 -= 1f)
			{
				num5 = MapCenterY + Map_VerticalScale_sec * num25;
				if (((num5 > num11 + 10f) & (num5 < num12 - graphics.MeasureString("I", font).Height)) && num25 != 0f)
				{
					string text3 = $"{0f - num25:+#.0;-#.0}\"";
					float num24 = graphics.MeasureString(text3, font).Width;
					graphics.DrawLine(pen, MapLeft, num5, MapLeft + num14, num5);
					graphics.DrawString(text3, font, black, MapLeft + num14 + 2f, num5 - 6f);
					graphics.DrawLine(pen, MapRight, num5, MapRight - num14, num5);
					graphics.DrawString(text3, font, black, MapRight - num24 - num14 - 2f, num5 - 7f);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			Limb_Plot.picLimb.set_Image((Image)image);
			graphics.Dispose();
			Limb_Plot.Updating = false;
			((Control)Limb_Plot.picLimb).Refresh();
		}

		internal static void DrawEclipseImage()
		{
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = 0f;
			double num5 = 932.58;
			int width = ((Control)Eclipse_Image.picEclipse).get_Width();
			int height = ((Control)Eclipse_Image.picEclipse).get_Height();
			Bitmap image = new Bitmap(width, height);
			Font font = new Font("Times New Roman", 8f);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.Black);
			Image_CenterX = (float)width / 2f;
			Image_RadialScale = (float)((double)height * Image_Scale / MoonR);
			Image_CenterY = (float)((double)height * (0.25 + Image_Scale));
			double num6 = Math.Atan2(SunMoonX, SunMoonY);
			double num7 = Math.Sqrt(SunMoonX * SunMoonX + SunMoonY * SunMoonY);
			double num8 = 1.1 * Math.Floor(Math.Atan((double)Image_CenterX / MoonR / (double)Image_RadialScale) * (180.0 / Math.PI));
			if (Image_Scale < 0.5)
			{
				Image_CenterY = height / 2;
				num8 = 181.0;
			}
			ArrayList arrayList = new ArrayList();
			for (double num9 = 0.0 - num8; num9 <= num8; num9 += 0.1)
			{
				double num10 = ((!BailyBeads_Main.optLOLA.get_Checked()) ? 0.0 : Utilities.LOLA_LimbHeight_ActualDistance(CentralAxisAngle + num9, L, B, MoonR / 932.58));
				double num11 = (CentralAxisAngle + num9 + C) / (180.0 / Math.PI);
				float num12 = (float)((MoonR + MoonScale * num10) * (double)Image_RadialScale * Math.Sin(num9 / (180.0 / Math.PI)));
				if (Mirror)
				{
					num12 = 0f - num12;
				}
				float num13 = (float)((MoonR + MoonScale * num10) * (double)Image_RadialScale * Math.Cos(num9 / (180.0 / Math.PI)));
				num5 = (float)Math.Sqrt(num12 * num12 + num13 * num13);
				if (SunR < num7)
				{
					break;
				}
				double num14 = num7 * Math.Cos(num11 - num6) + Math.Sqrt(SunR * SunR - num7 * num7 * Math.Sin(num11 - num6) * Math.Sin(num11 - num6));
				float num15 = (float)(num14 * (double)Image_RadialScale * Math.Sin(num9 / (180.0 / Math.PI)));
				if (Mirror)
				{
					num15 = 0f - num15;
				}
				float num16 = (float)(num14 * (double)Image_RadialScale * Math.Cos(num9 / (180.0 / Math.PI)));
				double num17 = (float)Math.Sqrt(num15 * num15 + num16 * num16);
				if (num9 > 0.0 - num8 && num17 > num5)
				{
					arrayList.Clear();
					arrayList.Add(new PointF(Image_CenterX - num12, Image_CenterY - num13));
					arrayList.Add(new PointF(Image_CenterX - num15, Image_CenterY - num16));
					arrayList.Add(new PointF(Image_CenterX - num2, Image_CenterY - num4));
					arrayList.Add(new PointF(Image_CenterX - num, Image_CenterY - num3));
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					graphics.FillPolygon(Brushes.Yellow, points);
				}
				if (Math.Abs(num14 - MoonR - num10) > 10.0)
				{
					num9 += 1.0;
				}
				else if (Math.Abs(num14 - MoonR - num10) > 5.0)
				{
					num9 += 0.4;
				}
				num = num12;
				num3 = num13;
				num2 = num15;
				num4 = num16;
			}
			float num18 = graphics.MeasureString(GIF_FileDate + " : " + GIF_PlotTime, font).Width / 2f;
			graphics.DrawString(GIF_FileDate + " : " + GIF_PlotTime, font, Brushes.White, (float)(width / 2) - num18, height - 35);
			num18 = graphics.MeasureString(GIF_Location, font).Width / 2f;
			graphics.DrawString(GIF_Location, font, Brushes.White, (float)(width / 2) - num18, height - 20);
			graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, Brushes.Gray, 5f, 5f);
			Eclipse_Image.picEclipse.set_Image((Image)image);
			graphics.Dispose();
		}

		internal static void CursorLocationOnEclipseImage(double X, double Y)
		{
			double num = 1.0;
			if (Mirror)
			{
				num = -1.0;
			}
			CentralAxisAngle = (double)BailyBeads_Main.updnAxisAngle.get_Value();
			if (Image_RadialScale > 0f)
			{
				if ((double)Image_CenterY == Y)
				{
					Y = 1.0;
				}
				double num2 = CentralAxisAngle + Math.Atan2(num * ((double)Image_CenterX - X), (double)Image_CenterY - Y) * (180.0 / Math.PI);
				double num3 = Math.Sqrt(((double)Image_CenterX - X) * ((double)Image_CenterX - X) + ((double)Image_CenterY - Y) * ((double)Image_CenterY - Y)) / (double)Image_RadialScale - MoonR;
				if (Math.Abs(num3) < 10.0)
				{
					((Control)BailyBeads_Main.txtCursorAA).set_Text(string.Format("{0,2:F2}", num2));
					((Control)BailyBeads_Main.txtCursorHeight).set_Text(string.Format("{0,2:F2}", num3));
				}
				else
				{
					((Control)BailyBeads_Main.txtCursorAA).set_Text("");
					((Control)BailyBeads_Main.txtCursorHeight).set_Text("");
				}
			}
		}

		internal static void PlotLimbAtCursorLocation(double X, double Y)
		{
			double num = 1.0;
			if (Mirror)
			{
				num = -1.0;
			}
			CentralAxisAngle = (double)BailyBeads_Main.updnAxisAngle.get_Value();
			if (Image_RadialScale > 0f)
			{
				if ((double)Image_CenterY == Y)
				{
					Y = 1.0;
				}
				double num2 = CentralAxisAngle + Math.Atan2(num * ((double)Image_CenterX - X), (double)Image_CenterY - Y) * (180.0 / Math.PI);
				if (num2 < 0.0)
				{
					num2 += 360.0;
				}
				if (num2 > 360.0)
				{
					num2 -= 360.0;
				}
				CentralAxisAngle = num2;
				Show_Limb_Plot();
				DrawLunarLimb();
			}
		}

		internal static void Solve()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			int num9 = 0;
			for (int i = 0; i < Observations.Count; i++)
			{
				if (Observations[i].Check)
				{
					double aA = Observations[i].AA;
					double finalLimbSeparation = GetFinalLimbSeparation(((double)Observations[i].Hr + (double)Observations[i].Min / 60.0 + Observations[i].Sec / 3600.0) / 24.0, aA);
					double num10 = Math.Sin((aA + C) / (180.0 / Math.PI));
					double num11 = Math.Cos((aA + C) / (180.0 / Math.PI));
					num += num10;
					num2 += num10 * num10;
					num3 += num10 * finalLimbSeparation;
					num4 += num11;
					num5 += num11 * num11;
					num6 += num11 * finalLimbSeparation;
					num7 += num10 * num11;
					num8 += finalLimbSeparation;
					num9++;
				}
			}
			if (num9 < 3)
			{
				((Control)Results.lblSolution).set_Text("[Sun] :");
				return;
			}
			double num12 = num2 - num * num / (double)num9;
			double num13 = num7 - num * num4 / (double)num9;
			double num14 = num3 - num * num8 / (double)num9;
			double num15 = num7 - num4 * num / (double)num9;
			double num16 = num5 - num4 * num4 / (double)num9;
			double num17 = num6 - num4 * num8 / (double)num9;
			double num18 = (num14 - num17 * num13 / num16) / (num12 - num13 * num15 / num16);
			double num19 = (num14 - num12 * num18) / num13;
			double num20 = (num8 - num * num18 - num4 * num19) / (double)num9;
			((Control)Results.lblSolution).set_Text("[Sun] : dRA= " + string.Format("{0,2:F2}", 0.0 - num18) + "  dDec= " + string.Format("{0,2:F2}", 0.0 - num19) + "  dR= " + string.Format("{0,2:F2}", 0.0 - num20));
		}

		private static double GetFinalLimbSeparation(double PlotTime, double AxisAngle)
		{
			if (!int.TryParse(((Control)BailyBeads_Main.txtYear).get_Text(), out var result))
			{
				result = 1900;
			}
			if (!int.TryParse(((Control)BailyBeads_Main.txtMonth).get_Text(), out var result2))
			{
				result2 = 1;
			}
			if (!double.TryParse(((Control)BailyBeads_Main.txtDay).get_Text(), out var result3))
			{
				result3 = 1.0;
			}
			result3 = Math.Floor(result3);
			double num = Utilities.JD_from_Date(result, result2, result3);
			if (!double.TryParse(((Control)BailyBeads_Main.txtLongDDD).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (!double.TryParse(((Control)BailyBeads_Main.txtLatDDD).get_Text(), out var result5))
			{
				result5 = 0.0;
			}
			if (!double.TryParse(((Control)BailyBeads_Main.txtAlt_m).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			Utilities.EarthOrientationParameters(num, out var x, out var y, out var _);
			result4 += (x * Math.Cos(result4 / (180.0 / Math.PI)) - y * Math.Sin(result4 / (180.0 / Math.PI))) / 3600.0;
			result5 += (x * Math.Sin(result4 / (180.0 / Math.PI)) + y * Math.Cos(result4 / (180.0 / Math.PI))) * Math.Tan(result5 / (180.0 / Math.PI)) / 3600.0;
			Utilities.TopocentricMoon(num + PlotTime, result4 / (180.0 / Math.PI), result5 / (180.0 / Math.PI), result6, Altitude_is_MSL: true, 0, out var RA, out var Dec, out var MoonRadius_Radians, out var MoonScale, out var l, out var b, out var C, out var _);
			Utilities.TopocentricSun(num + PlotTime, result4 / (180.0 / Math.PI), result5 / (180.0 / Math.PI), result6, out var raPlanet, out var decPlanet, out var PlanetRadius, out var _);
			double num2 = (raPlanet - RA) * (180.0 / Math.PI) * 3600.0 * Math.Cos((Dec + decPlanet) / 2.0);
			double num3 = (decPlanet - Dec) * (180.0 / Math.PI) * 3600.0;
			double num4 = MoonRadius_Radians * (180.0 / Math.PI) * 3600.0;
			double num5 = PlanetRadius * (180.0 / Math.PI) * 3600.0;
			double num6 = Math.Atan2(num2, num3);
			double num7 = Math.Sqrt(num2 * num2 + num3 * num3);
			double num8 = Utilities.LOLA_LimbHeight_ActualDistance(AxisAngle, l, b, MoonScale);
			double num9 = (AxisAngle + C) / (180.0 / Math.PI);
			return num7 * Math.Cos(num9 - num6) + Math.Sqrt(num5 * num5 - num7 * num7 * Math.Sin(num9 - num6) * Math.Sin(num9 - num6)) - num4 - num8;
		}
	}
}
