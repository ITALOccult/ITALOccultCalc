using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Mapping
{
	internal class LOLAHiRes
	{
		private static LunarLimbMap LunarLimbMap;

		private const double Radian = 180.0 / Math.PI;

		private const double RadianArcSecs = 648000.0 / Math.PI;

		private const double LunarRadius = 1737.4;

		private const double MeanEarthDistance = 384398.55;

		private const double MeanLunarRadius = 1738.0908063412;

		private const int PperDegree = 100;

		private const int DperDegree = 50;

		private const int NumDCoords = 2001;

		internal const int P_HalfRange = 3;

		internal const int D_HalfRange = 6;

		internal const int NumPpoints = 600;

		internal const int NumDpoints = 600;

		internal static int Dresolution = 2;

		internal static float HeightResolution = 0.01f;

		internal static float MapScale = 1f;

		internal static double Pclose;

		internal static double Pfar;

		private static double P_Limb_Offset = 0.0;

		private static double D_Limb_Offset = 0.0;

		private static double Limb_Height = 0.0;

		private static short[] MapHeights;

		internal static bool LastShowLimbPoint = true;

		private static int ChartWidth = 100;

		private static int ChartHeight = 100;

		private static float Xinterval = 1f;

		private static float Yinterval = 1f;

		private static double PLimb_deg = 0.0;

		private static double DLimb_deg = 0.0;

		private static double L_LimbPlotDeg = 0.0;

		private static double B_LimbPlot_Deg = 0.0;

		private static Points LimbPoint;

		private static List<LOLApoints> PointsOnLimb = new List<LOLApoints>();

		private static double sinL = 0.0;

		private static double cosL = 1.0;

		private static double sinB = 0.0;

		private static double cosB = 1.0;

		private static double ActualEarthDistance = 384398.55;

		private static double MoonRadiusSupplement = 1.57;

		private static double MoonRadiusArcSec = 932.58;

		internal static void Set_Libration_Distance_Parameters(double MoonRatioToMean, double L_deg, double B_deg)
		{
			sinL = Math.Sin((0.0 - L_deg) / (180.0 / Math.PI));
			cosL = Math.Cos(L_deg / (180.0 / Math.PI));
			sinB = Math.Sin((0.0 - B_deg) / (180.0 / Math.PI));
			cosB = Math.Cos(B_deg / (180.0 / Math.PI));
			ActualEarthDistance = 384398.55 / MoonRatioToMean;
			MoonRadiusSupplement = Math.PI / 2.0 - Math.Asin(1738.0908063412 / ActualEarthDistance);
			MoonRadiusArcSec = 648000.0 / Math.PI * Math.Asin(1738.0908063412 / ActualEarthDistance);
		}

		private static short CorrectHeight(long RecordNum)
		{
			return RecordNum switch
			{
				1994041L => -596, 
				3749388L => -502, 
				34280521L => -4050, 
				34282522L => -2888, 
				35274946L => -5853, 
				35276946L => -607, 
				36859660L => -576, 
				37657424L => -713, 
				_ => 0, 
			};
		}

		internal static void GetLimbData(double Ldeg, double Bdeg, double AAstart_deg, double AAend_deg, double MoonRatioToMean, List<Points> Limb)
		{
			if (Ldeg == 0.0 && Bdeg == 0.0)
			{
				Bdeg = 0.0001;
			}
			if (AAstart_deg < 0.0)
			{
				AAstart_deg += 360.0;
			}
			if (AAstart_deg >= 360.0)
			{
				AAstart_deg -= 360.0;
			}
			if (AAend_deg < 0.0)
			{
				AAend_deg += 360.0;
			}
			if (AAend_deg >= 360.0)
			{
				AAend_deg -= 360.0;
			}
			if (AAstart_deg > AAend_deg)
			{
				AAstart_deg -= 360.0;
			}
			Set_Libration_Distance_Parameters(MoonRatioToMean, Ldeg, Bdeg);
			bool flag = false;
			_ = new double[3601];
			Limb.Clear();
			int num = 36000;
			Utilities.Librations_P_D(Ldeg, Bdeg, AAstart_deg, out var P_deg, out var D_deg);
			P_deg -= 0.5;
			if (P_deg < 0.0)
			{
				P_deg += 360.0;
			}
			int num2 = Convert.ToInt32(P_deg * 100.0);
			Utilities.Librations_P_D(Ldeg, Bdeg, AAend_deg, out P_deg, out var D_deg2);
			P_deg += 0.5;
			if (P_deg >= 360.0)
			{
				P_deg -= 360.0;
			}
			int num3 = Convert.ToInt32(P_deg * 100.0);
			if (D_deg > D_deg2)
			{
				Utilities.Swap(ref D_deg, ref D_deg2);
			}
			D_deg -= 7.0;
			D_deg2 += 7.0;
			if (D_deg < -20.0)
			{
				D_deg = -20.0;
			}
			if (D_deg2 > 20.0)
			{
				D_deg2 = 20.0;
			}
			int num4 = Convert.ToInt32((D_deg + 20.0) * 50.0);
			int num5 = Convert.ToInt32((D_deg2 + 20.0) * 50.0);
			flag = num3 < num2;
			int num6 = num2;
			int num7 = num3;
			if (flag)
			{
				num6 = 0;
				num7 = num3;
			}
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\LOLA128.bin", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				for (int i = 0; i <= 1; i++)
				{
					for (int j = num4; j <= num5; j++)
					{
						double d = ((double)j / 50.0 - 20.0) / (180.0 / Math.PI);
						for (int k = num6; k < num7; k++)
						{
							P_deg = (double)k / 100.0 / (180.0 / Math.PI);
							long num8 = 2 * (j + 2001 * k);
							fileStream.Seek(num8, SeekOrigin.Begin);
							short num9 = binaryReader.ReadInt16();
							num9 = (short)(num9 + CorrectHeight(num8 / 2));
							if (Convert_PDh_toAAh(P_deg, d, num9, out var AA_deg, out var r, out var Dlimb_deg))
							{
								LimbPoint = new Points();
								if (AA_deg - AAstart_deg > 360.0)
								{
									AA_deg -= 360.0;
								}
								LimbPoint.AA = AA_deg;
								LimbPoint.H = r / MoonRatioToMean;
								LimbPoint.D = Dlimb_deg;
								Limb.Add(LimbPoint);
							}
						}
					}
					if (flag)
					{
						num6 = num2;
						num7 = num;
						continue;
					}
					break;
				}
			}
			Limb.Sort();
		}

		internal static bool Convert_PDh_toAAh(double P, double D, int HeightFromFile, out double AA_deg, out double r, out double Dlimb_deg)
		{
			double num = 1737.4 + (double)HeightFromFile / 1000.0;
			AA_deg = 0.0;
			r = 0.0;
			double num2 = num * Math.Cos(D) * Math.Cos(P);
			double num3 = num * Math.Cos(D) * Math.Sin(P);
			double num4 = num * Math.Sin(D);
			double num5 = num3 * cosL - num4 * sinL;
			double num6 = num3 * sinL + num4 * cosL;
			double num7 = num2 * cosB + num6 * sinB;
			double num8 = num5;
			double num9 = (0.0 - num2) * sinB + num6 * cosB;
			double x = ActualEarthDistance - num9;
			double y = Math.Sqrt(num7 * num7 + num8 * num8);
			Dlimb_deg = (MoonRadiusSupplement - Math.Atan2(y, num9)) * (180.0 / Math.PI);
			r = Math.Atan2(y, x) * (648000.0 / Math.PI) - MoonRadiusArcSec;
			AA_deg = Math.Atan2(num8, num7) * (180.0 / Math.PI);
			if (AA_deg < 0.0)
			{
				AA_deg += 360.0;
			}
			if (r > -3.0)
			{
				return true;
			}
			return false;
		}

		internal static double LimbHeight_Slope(double AxisAngle_deg, double Ldeg, double Bdeg, double MoonRatioToMean, bool IncludeSlope, bool WideSlope, out double SlopeBefore_Deg, out double SlopeAfter_Deg, out double P_Limb_deg, out double D_Limb_deg)
		{
			double AA_deg = 0.0;
			double Dlimb_deg = 0.0;
			double r = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = -10.0;
			double num5 = 0.01;
			if (WideSlope)
			{
				num5 = 0.04;
			}
			int num6 = -1;
			int num7 = -1;
			int num8 = 0;
			int num9 = 0;
			short num10 = 0;
			int num11 = 28;
			double[] array = new double[num11 + 1];
			double[] array2 = new double[3] { -10.0, -10.0, -10.0 };
			int[] array3 = new int[num11 + 1];
			while (AxisAngle_deg < 0.0)
			{
				AxisAngle_deg += 360.0;
			}
			while (AxisAngle_deg > 359.995)
			{
				AxisAngle_deg -= 360.0;
			}
			Convert.ToInt32(AxisAngle_deg * 100.0);
			SlopeBefore_Deg = (SlopeAfter_Deg = (P_Limb_deg = (D_Limb_deg = 0.0)));
			Set_Libration_Distance_Parameters(MoonRatioToMean, Ldeg, Bdeg);
			Utilities.Librations_P_D(Ldeg, Bdeg, AxisAngle_deg, out var P_deg, out var D_deg);
			Convert_PDh_toAAh(P_deg / (180.0 / Math.PI), (D_deg - 5.0) / (180.0 / Math.PI), 0, out var AA_deg2, out r, out var _);
			if (AxisAngle_deg - AA_deg2 > 50.0)
			{
				AA_deg2 += 360.0;
			}
			if (AxisAngle_deg - AA_deg2 < -50.0)
			{
				AA_deg2 -= 360.0;
			}
			double num12 = P_deg + (AxisAngle_deg - AA_deg2);
			Convert_PDh_toAAh(P_deg / (180.0 / Math.PI), (D_deg + 5.0) / (180.0 / Math.PI), 0, out var AA_deg3, out r, out var _);
			if (AxisAngle_deg - AA_deg3 > 50.0)
			{
				AA_deg3 += 360.0;
			}
			if (AxisAngle_deg - AA_deg3 < -50.0)
			{
				AA_deg3 -= 360.0;
			}
			num3 = (P_deg + (AxisAngle_deg - AA_deg3) - num12) / 10.0;
			int num13 = Convert.ToInt32((D_deg + 20.0 - 7.0) * 50.0);
			int num14 = Convert.ToInt32((D_deg + 20.0 + 7.0) * 50.0);
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\LOLA128.bin", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				for (int i = num13; i <= num14; i += 25)
				{
					int num15 = Convert.ToInt32((P_deg + num3 * ((double)i / 50.0 - 20.0 - D_deg)) * 100.0);
					if (num15 < 0)
					{
						num15 += 36000;
					}
					if (num15 >= 36000)
					{
						num15 -= 36000;
					}
					num = (double)i / 50.0 - 20.0;
					num2 = (double)num15 / 100.0;
					long num16 = 2 * (i + 2001 * num15);
					fileStream.Seek(num16, SeekOrigin.Begin);
					num10 = binaryReader.ReadInt16();
					num10 = (short)(num10 + CorrectHeight(num16 / 2));
					if (Convert_PDh_toAAh(num2 / (180.0 / Math.PI), num / (180.0 / Math.PI), num10, out AA_deg, out r, out Dlimb_deg))
					{
						array[num8] = r;
						if (array[num8] > num4)
						{
							num4 = r;
							num9 = num8;
						}
					}
					else
					{
						array[num8] = -4.0;
					}
					array3[num8] = i;
					num8++;
				}
				num6 = ((num9 <= 1) ? array3[0] : array3[num9 - 2]);
				num8--;
				array2[0] = (array2[1] = (array2[2] = num4 - 0.1));
				for (int j = 0; j < num9; j++)
				{
					if (array[j] > num4 - 0.5)
					{
						num6 = ((j != 0) ? array3[j - 1] : array3[0]);
						break;
					}
				}
				num7 = ((num9 <= num11 - 2) ? array3[num9 + 2] : array3[num11]);
				for (int num17 = num8; num17 > num9 + 1; num17--)
				{
					if (array[num17] > num4 - 0.5)
					{
						num7 = ((num17 != num11) ? array3[num17 + 1] : array3[num11]);
						break;
					}
				}
				for (int k = -1; k <= 1; k++)
				{
					if (!IncludeSlope)
					{
						k++;
					}
					for (int i = num6; i <= num7; i++)
					{
						int num15 = Convert.ToInt32((P_deg + num5 * (double)k + num3 * ((double)i / 50.0 - 20.0 - D_deg)) * 100.0);
						if (num15 < 0)
						{
							num15 += 36000;
						}
						if (num15 >= 36000)
						{
							num15 -= 36000;
						}
						num = (double)i / 50.0 - 20.0;
						num2 = (double)num15 / 100.0;
						long num16 = 2 * (i + 2001 * num15);
						fileStream.Seek(num16, SeekOrigin.Begin);
						num10 = binaryReader.ReadInt16();
						num10 = (short)(num10 + CorrectHeight(num16 / 2));
						Convert_PDh_toAAh(num2 / (180.0 / Math.PI), num / (180.0 / Math.PI), num10, out AA_deg, out r, out Dlimb_deg);
						if (r > array2[k + 1])
						{
							array2[k + 1] = r;
							P_Limb_deg = num2;
							D_Limb_deg = num;
							Limb_Height = num10;
						}
						else if (array2[k + 1] - r > 2.0)
						{
							i += 19;
						}
						else if (array2[k + 1] - r > 1.0)
						{
							i += 9;
						}
						else if (array2[k + 1] - r > 0.5)
						{
							i += 4;
						}
					}
					if (!IncludeSlope)
					{
						break;
					}
				}
			}
			if (IncludeSlope)
			{
				SlopeBefore_Deg = 180.0 / Math.PI * Math.Atan2(array2[0] - array2[1], num5 / (180.0 / Math.PI) * MoonRadiusArcSec);
				SlopeAfter_Deg = 180.0 / Math.PI * Math.Atan2(array2[1] - array2[2], num5 / (180.0 / Math.PI) * MoonRadiusArcSec);
			}
			return array2[1];
		}

		internal static void MapLimbRegion(double Ldeg, double Bdeg, double AAmid_deg, bool ShowLimbPoint, double P_LimbContactPoint_deg, double D_LimbContactPoint_deg)
		{
			int num = 0;
			PointsOnLimb.Clear();
			MapHeights = new short[361201];
			L_LimbPlotDeg = Ldeg;
			B_LimbPlot_Deg = Bdeg;
			Set_Libration_Distance_Parameters(1.0, Ldeg, Bdeg);
			Utilities.Librations_P_D(Ldeg, Bdeg, AAmid_deg, out var P_deg, out var D_deg);
			PLimb_deg = P_deg;
			DLimb_deg = D_deg;
			Convert_PDh_toAAh(P_deg / (180.0 / Math.PI), (D_deg - 6.0) / (180.0 / Math.PI), 0, out var AA_deg, out var r, out var _);
			double num2 = 0.0;
			if (AA_deg - AAmid_deg < -90.0)
			{
				num2 = 360.0;
			}
			if (AA_deg - AAmid_deg > 90.0)
			{
				num2 = -360.0;
			}
			Pfar = 300.0 + (AA_deg - AAmid_deg + num2) * 100.0;
			Convert_PDh_toAAh(P_deg / (180.0 / Math.PI), (D_deg + 6.0) / (180.0 / Math.PI), 0, out var AA_deg2, out r, out var _);
			num2 = 0.0;
			if (AA_deg2 - AAmid_deg < -90.0)
			{
				num2 = 360.0;
			}
			if (AA_deg2 - AAmid_deg > 90.0)
			{
				num2 = -360.0;
			}
			Pclose = 300.0 + (AA_deg2 - AAmid_deg + num2) * 100.0;
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\LOLA128.bin", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				num = 0;
				for (double num3 = P_deg - 3.0; num3 < P_deg + 3.0; num3 += 0.01)
				{
					long num4 = ((num3 < 0.0) ? Convert.ToInt32((num3 + 360.0) * 100.0) : ((!(num3 > 359.995)) ? Convert.ToInt32(num3 * 100.0) : Convert.ToInt32((num3 - 360.0) * 100.0)));
					if (num4 == 36000)
					{
						num4 = 0L;
					}
					long num5 = Convert.ToInt32((D_deg - 6.0 + 20.0) * 50.0);
					long offset = 2 * (num5 + 2001 * num4);
					fileStream.Seek(offset, SeekOrigin.Begin);
					for (int i = 0; i < 600; i++)
					{
						MapHeights[num + i * 600] = binaryReader.ReadInt16();
					}
					num++;
				}
			}
			for (double num6 = AAmid_deg - 3.0; num6 < AAmid_deg + 3.0; num6 += 0.02)
			{
				LOLApoints lOLApoints = new LOLApoints();
				lOLApoints.Ht = LimbHeight_Slope(num6, Ldeg, Bdeg, 1.0, IncludeSlope: false, WideSlope: false, out var _, out var _, out var P_Limb_deg, out var D_Limb_deg);
				lOLApoints.P = P_Limb_deg;
				lOLApoints.D = D_Limb_deg;
				PointsOnLimb.Add(lOLApoints);
			}
			ShowLunarLimbMap();
			P_Limb_Offset = (P_LimbContactPoint_deg - P_deg + 3.0) * 100.0;
			D_Limb_Offset = (D_LimbContactPoint_deg - D_deg + 6.0) * 50.0;
			LastShowLimbPoint = ShowLimbPoint;
			DrawLimbMap(ShowLimbPoint, ShowLimbPoints: true, ShowLimbHeights: false);
		}

		internal static void DrawLimbMap(bool ShowLimbPoint, bool ShowLimbPoints, bool ShowLimbHeights)
		{
			ChartWidth = ((Control)LunarLimbMap.picMoonMap).get_Width();
			ChartHeight = ((Control)LunarLimbMap.picMoonMap).get_Height();
			Bitmap image = new Bitmap(ChartWidth, ChartHeight);
			Graphics graphics = Graphics.FromImage(image);
			ArrayList arrayList = new ArrayList();
			Xinterval = (float)ChartWidth / 600f;
			Yinterval = (float)ChartHeight / 600f;
			float num = ChartWidth / 3;
			float num2 = (float)(ChartHeight / 6) / 2f;
			Pen pen = new Pen(Brushes.Black, 0.5f);
			Pen pen2 = new Pen(Brushes.Green, 2f);
			pen2.DashPattern = new float[2] { 3f, 6f };
			new Pen(Brushes.Red, 2f);
			Pen pen3 = new Pen(Brushes.DarkCyan, 0.5f);
			Pen pen4 = new Pen(Brushes.Red, 1f);
			graphics.Clear(Color.LightYellow);
			for (int i = 0; i < 600; i += Dresolution)
			{
				arrayList.Clear();
				for (int j = 0; j < 600; j++)
				{
					arrayList.Add(new PointF((float)ChartWidth - Xinterval * (float)j, Yinterval * (float)i - HeightResolution * (float)MapHeights[j + i * 600]));
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				graphics.DrawLines(pen, points);
			}
			if (ShowLimbPoints)
			{
				for (int k = 0; k < PointsOnLimb.Count; k++)
				{
					float num3 = (float)((double)num2 * (PointsOnLimb[k].D - DLimb_deg) + (double)(ChartHeight / 2));
					float num4 = 0f;
					if (PointsOnLimb[k].P - PLimb_deg < -90.0)
					{
						num4 = 360f;
					}
					if (PointsOnLimb[k].P - PLimb_deg > 90.0)
					{
						num4 = -360f;
					}
					float num5 = (float)((double)ChartWidth - (double)(num / 2f) * (PointsOnLimb[k].P - PLimb_deg + (double)num4 + 3.0));
					graphics.FillEllipse(Brushes.Red, num5, num3 - HeightResolution * 1863f * (float)PointsOnLimb[k].Ht, 4f, 4f);
					if (ShowLimbHeights)
					{
						graphics.DrawLine(pen3, num5, num3 - HeightResolution * 1863f * (float)PointsOnLimb[k].Ht, num5, num3);
					}
				}
			}
			graphics.DrawLine(pen2, (float)((double)Xinterval * Pclose), ChartHeight - 2, (float)((double)Xinterval * Pfar), 2f);
			graphics.FillRectangle(Brushes.Wheat, 4, 5, 131, 22);
			graphics.DrawRectangle(pen, 4, 5, 130, 22);
			double num6 = PLimb_deg - 3.0;
			if (num6 >= 360.0)
			{
				num6 -= 360.0;
			}
			if (num6 < 0.0)
			{
				num6 += 360.0;
			}
			double num7 = PLimb_deg + 3.0;
			if (num7 < 0.0)
			{
				num7 += 360.0;
			}
			string s = string.Format("P from {0,1:f2}° to {1,1:f2}°", num6, num7);
			string s2 = string.Format("D from {0,1:f2}° to {1,1:f2}°", DLimb_deg - 6.0, DLimb_deg + 6.0);
			graphics.DrawString(s, new Font("Arial", 8f), Brushes.DarkBlue, 4f, 4f);
			graphics.DrawString(s2, new Font("Arial", 8f), Brushes.DarkBlue, 4f, 14f);
			graphics.DrawImage(Resources.Upload_32x, (float)ChartWidth / 2f - 10f, 2f);
			graphics.DrawImage(Resources.Upload_32x, (float)ChartWidth / 2f - 10f, ChartHeight - 40);
			graphics.DrawRectangle(pen4, 0, 0, ChartWidth - 1, ChartHeight - 1);
			LunarLimbMap.picMoonMap.set_Image((Image)image);
			graphics.Dispose();
			((Control)LunarLimbMap).Focus();
		}

		internal static string ToolTipText(float x, float y)
		{
			double moonRatioToMean = 1.0;
			double AA_deg = 0.0;
			double num = PLimb_deg + ((double)ChartWidth / 2.0 - (double)x) / (double)Xinterval / 100.0;
			if (num < 0.0)
			{
				num += 360.0;
			}
			if (num >= 360.0)
			{
				num -= 360.0;
			}
			double num2 = DLimb_deg - ((double)ChartHeight / 2.0 - (double)y) / (double)Yinterval / 50.0;
			Utilities.Convert_PD_toAA(L_LimbPlotDeg, B_LimbPlot_Deg, moonRatioToMean, num, num2, out AA_deg, out var HeightAboveLimb, out var _);
			return string.Format(string.Format("AA = {0,1:F2},  ", AA_deg) + "(P = {0,1:F2},  D = {1,1:F2})", num, num2) + string.Format(", Mean limb height = {0,1:F2}", HeightAboveLimb);
		}

		internal static void ShowLunarLimbMap()
		{
			try
			{
				((Control)LunarLimbMap).Show();
			}
			catch
			{
				LunarLimbMap = new LunarLimbMap();
				((Control)LunarLimbMap).Show();
			}
		}
	}
}
