using System;
using System.Collections;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Reflection;
using Occult.Properties;

namespace Occult
{
	internal class Maps
	{
		public const double Radian = 180.0 / Math.PI;

		public const double TwoPi = Math.PI * 2.0;

		public static string AppPath;

		private static float MercatorMapCentreX = 0f;

		private static float MercatorMapCentreY = 0f;

		private static float MercatorMapScale = 0f;

		private static float MercatorZeroLongitude = 0f;

		private static float MercatorMapCentreXOffset = 0f;

		private static float MercatorMapCentreYOffset = 0f;

		public static float GlobeMapCentreX = 0f;

		public static float GlobeMapCentreY = 0f;

		public static float GlobeMapScale = 0f;

		private static double GlobeZeroLongitude = 0.0;

		private static double GlobeZeroLatitude = 0.0;

		private static double cosL0;

		private static double sinL0;

		private static double Rho1;

		private static double sinD1;

		private static double cosD1;

		private static float MapCentreX = 0f;

		private static float MapCentreY = 0f;

		private static float MapScale = 10f;

		private static double MapZeroLongitude = 0.0;

		private static double MapZeroLatitude = 0.0;

		private static double Middle_RA;

		private static double Middle_Dec;

		private static float ChartCentre_X;

		private static float ChartCentre_Y;

		private static float ChartScale_PixelsPerDeg;

		private static bool StarMapDrawing = false;

		public static void PlotMercatorWorld(Graphics formGraphics, float Width, float Height, float ZeroLongitude_deg, bool BWflag, bool MultiPlot, int MultiPlotCount)
		{
			float num = 0f;
			float num2 = 0f;
			float x = 0f;
			float y = 0f;
			float x2 = 0f;
			float y2 = 0f;
			Pen pen = new Pen(Color.Red);
			Font font = new Font("Courier New", 8f);
			Color green = Color.Green;
			Color brown = Color.Brown;
			Brush white = Brushes.White;
			if (BWflag)
			{
				green = (brown = Color.Black);
				white = Brushes.Black;
			}
			else
			{
				green = Color.Green;
				brown = Color.Brown;
				white = Brushes.White;
			}
			Brush brush = white;
			ArrayList arrayList = new ArrayList();
			MercatorMapScale = Width / 430f;
			MercatorMapCentreX = Width / 2.05f;
			MercatorMapCentreY = Height / 1.9f;
			MercatorZeroLongitude = ZeroLongitude_deg % 360f;
			_ = Height / 2.4f;
			float num3 = 180f * MercatorMapScale;
			MercatorMapCentreXOffset = 0f;
			MercatorMapCentreYOffset = 0f;
			if (MultiPlot)
			{
				int num4 = MultiPlotCount / 3;
				MercatorMapCentreXOffset = (float)(MultiPlotCount % 3) * Width;
				MercatorMapCentreYOffset = (float)num4 * Height;
			}
			for (num2 = 90f; num2 >= -90f; num2 -= 30f)
			{
				MercatorXY(MercatorZeroLongitude, num2, ref x2, ref y2);
				MercatorXY(MercatorZeroLongitude + 359.99f, num2, ref x, ref y);
				pen.Color = brown;
				formGraphics.DrawLine(pen, x2, y2, x, y);
				brush = white;
				formGraphics.DrawString(Convert.ToString(num2), font, brush, x + 6f, y - 4f);
			}
			for (num = 0f; num <= 360f; num += 45f)
			{
				MercatorXY(num, 90f, ref x2, ref y2);
				MercatorXY(num, -90f, ref x, ref y);
				pen.Color = brown;
				formGraphics.DrawLine(pen, x2, y2, x, y);
				float num5 = num;
				if (num5 < 360f)
				{
					if (num5 > 180f)
					{
						num5 -= 360f;
					}
					brush = white;
					formGraphics.DrawString(Convert.ToString(num5), font, brush, x - 6f, y + 4f);
				}
			}
			pen.Color = brown;
			formGraphics.DrawLine(pen, MercatorMapCentreX + MercatorMapCentreXOffset - num3, MercatorMapCentreY + MercatorMapCentreYOffset + 90f * MercatorMapScale, MercatorMapCentreX + MercatorMapCentreXOffset - num3, MercatorMapCentreY + MercatorMapCentreYOffset - 90f * MercatorMapScale);
			formGraphics.DrawLine(pen, MercatorMapCentreX + MercatorMapCentreXOffset + num3, MercatorMapCentreY + MercatorMapCentreYOffset + 90f * MercatorMapScale, MercatorMapCentreX + MercatorMapCentreXOffset + num3, MercatorMapCentreY + MercatorMapCentreYOffset - 90f * MercatorMapScale);
			GraphicsPath graphicsPath = new GraphicsPath();
			pen.Color = green;
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\world.inx", FileMode.Open, FileAccess.Read);
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\World.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			for (int i = 1; i < 941; i++)
			{
				int num6 = binaryReader.ReadInt32();
				short num7 = binaryReader.ReadInt16();
				short num8 = binaryReader.ReadInt16();
				short num9 = binaryReader.ReadInt16();
				short num10 = binaryReader.ReadInt16();
				short num11 = binaryReader.ReadInt16();
				int num12 = num6 - 1;
				int num13 = num12 + num7 - 1;
				float num14 = (float)num8 / 90f;
				float num15 = (float)num9 / 90f;
				float num16 = (float)num10 / 90f;
				float num17 = (float)num11 / 90f;
				fileStream2.Seek(4 * num12, SeekOrigin.Begin);
				float num18 = Convert.ToSingle(binaryReader2.ReadInt16());
				float num19 = Convert.ToSingle(binaryReader2.ReadInt16());
				fileStream2.Seek(4 * num13, SeekOrigin.Begin);
				float num20 = Convert.ToSingle(binaryReader2.ReadInt16());
				float num21 = Convert.ToSingle(binaryReader2.ReadInt16());
				if (!((num18 != num20 || num19 != num21) | ((double)Math.Abs(num14 - num15) > 1.0 / Math.Cos(num19 / 5200f)) | (Math.Abs(num16 - num17) > 1f)))
				{
					continue;
				}
				arrayList.Clear();
				PointF[] points;
				for (int j = num12; j <= num13; j++)
				{
					fileStream2.Seek(4 * j, SeekOrigin.Begin);
					num = Convert.ToSingle(binaryReader2.ReadInt16());
					num2 = Convert.ToSingle(binaryReader2.ReadInt16());
					MercatorXY(num / 90f, num2 / 90f, ref x, ref y);
					if (((j > num12) & (Math.Sign(x - MercatorMapCentreX - MercatorMapCentreXOffset) != Math.Sign(x2 - MercatorMapCentreX - MercatorMapCentreXOffset))) && Math.Abs(x - x2) > num3)
					{
						arrayList.Add(new PointF(MercatorMapCentreX + MercatorMapCentreXOffset + num3 * (float)Math.Sign(x2 - MercatorMapCentreX - MercatorMapCentreXOffset), y));
						points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
						formGraphics.DrawLines(pen, points);
						arrayList.Clear();
						arrayList.Add(new PointF(MercatorMapCentreX + MercatorMapCentreXOffset - num3 * (float)Math.Sign(x2 - MercatorMapCentreX - MercatorMapCentreXOffset), y));
					}
					arrayList.Add(new PointF(x, y));
					x2 = x;
					y2 = y;
					if (j < num13 - 6)
					{
						j += 6;
					}
				}
				points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				formGraphics.DrawLines(pen, points);
			}
			graphicsPath.Dispose();
			fileStream2.Close();
			fileStream.Close();
		}

		public static void MercatorXY(float Longitude_deg, float Latitude_deg, ref float x, ref float y)
		{
			float num = Longitude_deg - MercatorZeroLongitude;
			if (num < 0f)
			{
				num += 360f;
			}
			if (num > 360f)
			{
				num -= 360f;
			}
			x = MercatorMapCentreX + (num - 180f) * MercatorMapScale + MercatorMapCentreXOffset;
			y = MercatorMapCentreY - Latitude_deg * MercatorMapScale + MercatorMapCentreYOffset;
		}

		public static void InverseMercatorXY(out float Longitude_deg, out float Latitude_deg, float x, float y, out bool Valid)
		{
			Longitude_deg = 0f;
			Latitude_deg = 0f;
			Valid = false;
			if (Math.Abs(x - MercatorMapCentreX) > 180f * MercatorMapScale)
			{
				Valid = false;
				return;
			}
			Latitude_deg = (MercatorMapCentreY - y) / MercatorMapScale;
			if (Math.Abs(Latitude_deg) > 90f)
			{
				Valid = false;
				return;
			}
			for (Longitude_deg = (x - MercatorMapCentreX) / MercatorMapScale + MercatorZeroLongitude + 180f; Longitude_deg < -180f; Longitude_deg += 360f)
			{
			}
			while (Longitude_deg > 180f)
			{
				Longitude_deg -= 360f;
			}
			Valid = true;
		}

		public static void EarthGlobe(Graphics formGraphics, float PlotScale, float xCenter, float yCenter, double ZeroLongitude_deg, double ZeroLatitude_deg, double SunLongitude_deg, double SunLatitude_deg, double SiteLongitude_deg, double SiteLatitude_deg, bool ShowSunLit, bool SiteCentered, bool PlotCities, bool FullResolution, bool BWflag, bool Mirrored)
		{
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = 0f;
			GlobeZeroLongitude = ZeroLongitude_deg / (180.0 / Math.PI);
			if (Math.Abs(GlobeZeroLongitude) > 360.0)
			{
				GlobeZeroLongitude = 0.0;
			}
			GlobeZeroLatitude = ZeroLatitude_deg / (180.0 / Math.PI);
			cosL0 = Math.Cos(GlobeZeroLatitude);
			sinL0 = Math.Sin(GlobeZeroLatitude);
			GlobeMapScale = PlotScale;
			if (GlobeMapScale <= 0f)
			{
				GlobeMapScale = 100f;
			}
			num = (num2 = (num3 = 0f));
			if (SiteCentered)
			{
				GlobeProjection(SiteLongitude_deg / (180.0 / Math.PI), SiteLatitude_deg / (180.0 / Math.PI), out num, out num2, out num3, Mirrored);
			}
			GlobeMapCentreX = xCenter - num;
			GlobeMapCentreY = yCenter + num2;
			Color color;
			Color color2;
			Color color3;
			Color color4;
			Color color5;
			Color color6;
			Color color7;
			Color color8;
			if (BWflag)
			{
				color = Color.Black;
				color2 = Color.Black;
				color3 = Color.Black;
				color4 = Color.Black;
				color5 = Color.Black;
				color6 = Color.Black;
				color7 = Color.Black;
				color8 = Color.Black;
			}
			else
			{
				color = Color.Lime;
				color2 = Color.Firebrick;
				color3 = Color.OliveDrab;
				color4 = Color.White;
				color5 = Color.Yellow;
				color6 = Color.MistyRose;
				color7 = Color.Gold;
				color8 = Color.White;
			}
			Pen pen = new Pen(color4);
			ArrayList arrayList = new ArrayList();
			double num5 = Math.Sin(ZeroLatitude_deg / (180.0 / Math.PI));
			double num6 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Cos(ZeroLatitude_deg / (180.0 / Math.PI));
			Rho1 = Math.Sqrt(num5 * num5 + num6 * num6);
			sinD1 = num5 / Rho1;
			cosD1 = num6 / Rho1;
			GlobeProjection(SiteLongitude_deg / (180.0 / Math.PI), SiteLatitude_deg / (180.0 / Math.PI), out num, out num2, out num3, Mirrored);
			if (num3 > 0f && ShowSunLit && SiteCentered)
			{
				pen.Color = color5;
				formGraphics.DrawLine(pen, GlobeMapCentreX + num - 5f, GlobeMapCentreY - num2, GlobeMapCentreX + num + 5f, GlobeMapCentreY - num2);
				formGraphics.DrawLine(pen, GlobeMapCentreX + num, GlobeMapCentreY - num2 - 5f, GlobeMapCentreX + num, GlobeMapCentreY - num2 + 5f);
			}
			pen.Color = color3;
			for (int i = -80; i <= 80; i += 20)
			{
				arrayList.Clear();
				int num7 = 10;
				if (FullResolution)
				{
					num7 = 3;
				}
				for (int j = 0; j <= 360; j += num7)
				{
					GlobeProjection((double)j / (180.0 / Math.PI), (double)i / (180.0 / Math.PI), out num, out num2, out num3, Mirrored);
					if (num3 > 0f)
					{
						num = GlobeMapCentreX + num;
						num2 = GlobeMapCentreY - num2;
						arrayList.Add(new PointF(num, num2));
					}
					else
					{
						if ((num4 > 0f) & (arrayList.Count > 1))
						{
							PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
							formGraphics.DrawLines(pen, points);
						}
						arrayList.Clear();
					}
					num4 = num3;
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					formGraphics.DrawLines(pen, points);
				}
			}
			for (int k = 0; k < 360; k += 30)
			{
				int num8 = 80;
				if (k % 90 == 0)
				{
					num8 = 90;
				}
				arrayList.Clear();
				int num7 = 10;
				if (FullResolution)
				{
					num7 = 3;
				}
				for (int l = -num8; l <= num8; l += num7)
				{
					GlobeProjection((double)k / (180.0 / Math.PI), (double)l / (180.0 / Math.PI), out num, out num2, out num3, Mirrored);
					if (num3 > 0f)
					{
						num = GlobeMapCentreX + num;
						num2 = GlobeMapCentreY - num2;
						arrayList.Add(new PointF(num, num2));
					}
					else
					{
						if ((num4 > 0f) & (arrayList.Count > 1))
						{
							PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
							formGraphics.DrawLines(pen, points);
						}
						arrayList.Clear();
					}
					num4 = num3;
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					formGraphics.DrawLines(pen, points);
				}
			}
			if (ShowSunLit)
			{
				double num9 = (90.0 - SunLatitude_deg) / (180.0 / Math.PI);
				double num10 = 1.0;
				int num11 = 4;
				if (!FullResolution)
				{
					num11 = 8;
					num10 = 200f / GlobeMapScale;
					if (num10 < 1.5)
					{
						num10 = 1.5;
					}
				}
				for (double num12 = -18.0; num12 < 90.0; num12 = ((num12 != -18.0) ? ((num12 != -12.0) ? ((num12 != -6.0) ? ((num12 != -0.8299999833106995) ? (num12 + num10) : 0.0) : (-0.8299999833106995)) : (-6.0)) : (-12.0)))
				{
					if (num12 >= 0.0)
					{
						pen.Color = color6;
					}
					else
					{
						pen.Color = color7;
					}
					double num13 = (90.0 - num12) / (180.0 / Math.PI);
					pen.DashStyle = DashStyle.Solid;
					if (num12 == -18.0)
					{
						pen.DashPattern = new float[2] { 1.5f, 15f };
					}
					if (num12 == -12.0)
					{
						pen.DashPattern = new float[2] { 4f, 6f };
					}
					if (num12 == -6.0)
					{
						pen.DashPattern = new float[2] { 2.5f, 2.5f };
					}
					if (num12 == -0.8299999833106995)
					{
						pen.Width = 2f;
					}
					else
					{
						pen.Width = 1f;
					}
					arrayList.Clear();
					for (double num14 = 0.0; num14 <= 360.0; num14 += (double)(num11 + 2 * Convert.ToInt32(num12 > 44.0)))
					{
						double num15 = num14 / (180.0 / Math.PI);
						num5 = Math.Sin(num13) * Math.Sin(num15);
						num6 = Math.Cos(num13) * Math.Sin(num9) - Math.Sin(num13) * Math.Cos(num9) * Math.Cos(num15);
						double num16 = Math.Cos(num13) * Math.Cos(num9) + Math.Sin(num13) * Math.Sin(num9) * Math.Cos(num15);
						double num17 = Math.Atan2(num5, num6) * (180.0 / Math.PI);
						double latitude_Rad = Math.Atan(num16 / Math.Sqrt(num5 * num5 + num6 * num6));
						GlobeProjection((SunLongitude_deg + num17) / (180.0 / Math.PI), latitude_Rad, out num, out num2, out num3, Mirrored);
						if (num3 > 0f)
						{
							num = GlobeMapCentreX + num;
							num2 = GlobeMapCentreY - num2;
							arrayList.Add(new PointF(num, num2));
						}
						else
						{
							if ((num4 > 0f) & (arrayList.Count > 1))
							{
								PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
								formGraphics.DrawLines(pen, points);
							}
							arrayList.Clear();
						}
						num4 = num3;
					}
					if (arrayList.Count > 1)
					{
						PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
						formGraphics.DrawLines(pen, points);
					}
				}
			}
			if (GlobeMapScale > 10f)
			{
				FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\world.inx", FileMode.Open, FileAccess.Read);
				FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\World.bin", FileMode.Open, FileAccess.Read);
				BinaryReader binaryReader = new BinaryReader(fileStream);
				BinaryReader binaryReader2 = new BinaryReader(fileStream2);
				int num18 = (int)(fileStream.Length / 14);
				pen.Color = color;
				pen.DashStyle = DashStyle.Solid;
				for (int m = 1; m <= num18; m++)
				{
					if (m == 941)
					{
						pen.Color = color2;
						if (!FullResolution)
						{
							break;
						}
						pen.DashPattern = new float[2] { 2f, 2f };
					}
					int num19 = binaryReader.ReadInt32();
					short num20 = binaryReader.ReadInt16();
					short num21 = binaryReader.ReadInt16();
					short num22 = binaryReader.ReadInt16();
					short num23 = binaryReader.ReadInt16();
					short num24 = binaryReader.ReadInt16();
					int num25 = num19 - 1;
					int num26 = num25 + num20 - 1;
					float num27 = (float)num21 / 90f;
					float num28 = (float)num22 / 90f;
					float num29 = (float)num23 / 90f;
					float num30 = (float)num24 / 90f;
					fileStream2.Seek(4 * num25, SeekOrigin.Begin);
					float num31 = Convert.ToSingle(binaryReader2.ReadInt16());
					float num32 = Convert.ToSingle(binaryReader2.ReadInt16());
					fileStream2.Seek(4 * num26, SeekOrigin.Begin);
					float num33 = Convert.ToSingle(binaryReader2.ReadInt16());
					float num34 = Convert.ToSingle(binaryReader2.ReadInt16());
					if (!(((num31 != num33 || num32 != num34) | ((double)Math.Abs(num27 - num28) > 1.0 / Math.Cos(num32 / 5200f)) | (Math.Abs(num29 - num30) > 1f)) || FullResolution))
					{
						continue;
					}
					arrayList.Clear();
					for (int n = num25; n <= num26; n++)
					{
						fileStream2.Seek(4 * n, SeekOrigin.Begin);
						float num35 = Convert.ToSingle(binaryReader2.ReadInt16());
						float num36 = Convert.ToSingle(binaryReader2.ReadInt16());
						GlobeProjection((double)(num35 / 90f) / (180.0 / Math.PI), (double)(num36 / 90f) / (180.0 / Math.PI), out num, out num2, out num3, Mirrored);
						if (num3 > 0f)
						{
							num = GlobeMapCentreX + num;
							num2 = GlobeMapCentreY - num2;
							arrayList.Add(new PointF(num, num2));
						}
						else
						{
							if ((num4 > 0f) & (arrayList.Count > 1))
							{
								PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
								formGraphics.DrawLines(pen, points);
							}
							arrayList.Clear();
						}
						num4 = num3;
						if ((double)num3 < 0.1)
						{
							n += 50;
							if ((double)num3 < 0.2)
							{
								n += 100;
							}
						}
						if (!FullResolution && n < num26 - 6)
						{
							n += 6;
						}
					}
					if (arrayList.Count > 1)
					{
						PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
						formGraphics.DrawLines(pen, points);
					}
				}
				fileStream.Close();
				fileStream2.Close();
			}
			pen.DashStyle = DashStyle.Solid;
			pen.Color = color8;
			formGraphics.DrawEllipse(pen, GlobeMapCentreX - GlobeMapScale, GlobeMapCentreY - GlobeMapScale, 2f * GlobeMapScale, 2f * GlobeMapScale);
		}

		public static void SetGlobeOrientation(double CentralLongitude_Rad, double CentralLatitude_Rad)
		{
			GlobeZeroLongitude = CentralLongitude_Rad;
			if (Math.Abs(GlobeZeroLongitude) > 360.0)
			{
				GlobeZeroLongitude = 0.0;
			}
			cosL0 = Math.Cos(CentralLatitude_Rad);
			sinL0 = Math.Sin(CentralLatitude_Rad);
			if (GlobeMapScale == 0f)
			{
				GlobeMapScale = 1f;
			}
		}

		public static void GlobeProjection(double Longitude_Rad, double Latitude_Rad, out float x, out float y, out float z, bool Mirrored)
		{
			double num = (float)(0.99832707 + 0.00167644 * Math.Cos(2.0 * Latitude_Rad));
			double num2 = 0.003358498 * Math.Sin(2.0 * Latitude_Rad);
			double num3 = Math.Cos(Longitude_Rad - GlobeZeroLongitude);
			double num4 = Math.Sin(Longitude_Rad - GlobeZeroLongitude);
			double num5 = Math.Cos(Latitude_Rad - num2);
			double num6 = Math.Sin(Latitude_Rad - num2);
			x = (float)(num * (double)GlobeMapScale * (num5 * num4));
			y = (float)(num * (double)GlobeMapScale * (num6 * cosL0 - num5 * sinL0 * num3));
			z = (float)(num * (double)GlobeMapScale * (num6 * sinL0 + num5 * cosL0 * num3));
			if (Mirrored)
			{
				x = 0f - x;
			}
		}

		public static void GlobeCoords(double SiteLongitude_Rad, double SiteLatitude_Rad, out float x, out float y, out float z, bool Mirrored)
		{
			GlobeProjection(SiteLongitude_Rad, SiteLatitude_Rad, out x, out y, out z, Mirrored);
			x /= GlobeMapScale;
			y /= GlobeMapScale;
			z /= GlobeMapScale;
		}

		public static void InverseGlobeCoords(out double SiteLongitude_deg, out double SiteLatitude_deg, float x, float y, out bool Valid)
		{
			SiteLongitude_deg = 0.0;
			SiteLatitude_deg = 0.0;
			x -= GlobeMapCentreX;
			x /= GlobeMapScale;
			y -= GlobeMapCentreY;
			y /= 0f - GlobeMapScale;
			double num = (double)y / Rho1;
			double num2 = (double)(1f - x * x) - num * num;
			if (num2 < 0.0)
			{
				Valid = false;
				return;
			}
			double num3 = x;
			double num4 = Math.Sqrt(num2);
			num2 = (0.0 - num) * sinD1 + num4 * cosD1;
			double num5 = num * cosD1 + num4 * sinD1;
			SiteLongitude_deg = (Math.Atan2(num3, num2) + GlobeZeroLongitude) * (180.0 / Math.PI);
			if (Math.Abs(SiteLongitude_deg) > 540.0)
			{
				SiteLongitude_deg = 0.0;
			}
			while (SiteLongitude_deg < -180.0)
			{
				SiteLongitude_deg += 360.0;
			}
			while (SiteLongitude_deg > 180.0)
			{
				SiteLongitude_deg -= 360.0;
			}
			SiteLatitude_deg = Math.Atan(num5 / Utilities.sqrt_1LessEarthEllipticitySqrd / Math.Sqrt(num3 * num3 + num2 * num2)) * (180.0 / Math.PI);
			Valid = true;
		}

		public static bool MapProjection(double Longitude_deg, double Latitude_deg, out float x, out float y)
		{
			if (Longitude_deg - MapZeroLongitude > 180.0)
			{
				Longitude_deg -= 360.0;
			}
			if (Longitude_deg - MapZeroLongitude < -180.0)
			{
				Longitude_deg += 360.0;
			}
			if (Math.Abs(MapZeroLatitude) < 10.0)
			{
				x = MapCentreX + MapScale * (float)((Longitude_deg - MapZeroLongitude) / (180.0 / Math.PI));
				y = MapCentreY - MapScale * (float)(Math.Cos(Latitude_deg / (180.0 / Math.PI) / 1000.0) * Math.Tan((Latitude_deg - MapZeroLatitude) / (180.0 / Math.PI)));
				return true;
			}
			if (Math.Abs(MapZeroLatitude) > 75.0)
			{
				if (Math.Sign(Latitude_deg) != Math.Sign(MapZeroLatitude))
				{
					x = 10f;
					y = 10f;
					return false;
				}
				x = MapCentreX + MapScale * (float)((Math.Abs(Latitude_deg / (180.0 / Math.PI)) - Math.PI / 2.0) * Math.Cos((Longitude_deg - MapZeroLongitude) / (180.0 / Math.PI)));
				y = MapCentreY - MapScale * (float)((Math.Abs(Latitude_deg / (180.0 / Math.PI)) - Math.PI / 2.0) * Math.Sin((Longitude_deg - MapZeroLongitude) / (180.0 / Math.PI)) * (double)Math.Sign(Latitude_deg) + (Math.Abs(MapZeroLatitude / (180.0 / Math.PI)) - Math.PI / 2.0));
				return true;
			}
			if (Math.Abs(Latitude_deg - MapZeroLatitude) < 80.0)
			{
				double num = (double)MapScale * (Math.Tan((Latitude_deg - MapZeroLatitude) / (180.0 / Math.PI)) - 1.0 / Math.Tan(MapZeroLatitude / (180.0 / Math.PI)));
				x = MapCentreX - (float)(num * Math.Sin((Longitude_deg - MapZeroLongitude) / (180.0 / Math.PI) * Math.Sin(MapZeroLatitude / (180.0 / Math.PI))));
				y = MapCentreY - (float)(num * Math.Cos((Longitude_deg - MapZeroLongitude) / (180.0 / Math.PI) * Math.Sin(MapZeroLatitude / (180.0 / Math.PI))) + (double)MapScale / Math.Tan(MapZeroLatitude / (180.0 / Math.PI)));
				return true;
			}
			x = -10f;
			y = -10f;
			return false;
		}

		public static void InverseMapProjection(out double Longitude_deg, out double Latitude_deg, float x, float y)
		{
			if (Math.Abs(MapZeroLatitude) < 10.0)
			{
				Longitude_deg = 180.0 / Math.PI * (double)(x - MapCentreX) / (double)MapScale + MapZeroLongitude;
				Latitude_deg = Math.Atan((MapCentreY - y) / MapScale) * (180.0 / Math.PI) + MapZeroLatitude;
				Latitude_deg = Math.Atan((double)((MapCentreY - y) / MapScale) / Math.Cos(Latitude_deg / (180.0 / Math.PI) / 1000.0)) * (180.0 / Math.PI) + MapZeroLatitude;
				while (Longitude_deg < -180.0)
				{
					Longitude_deg += 360.0;
				}
				while (Longitude_deg > 180.0)
				{
					Longitude_deg -= 360.0;
				}
				return;
			}
			if (Math.Abs(MapZeroLatitude) > 75.0)
			{
				double num = (x - MapCentreX) / MapScale;
				double num2 = (double)((MapCentreY - y) / MapScale) + (Math.Abs(MapZeroLatitude / (180.0 / Math.PI)) - Math.PI / 2.0);
				Latitude_deg = (double)Math.Sign(MapZeroLatitude) * (90.0 - Math.Sqrt(num * num + num2 * num2) * (180.0 / Math.PI));
				for (Longitude_deg = Math.Atan(num2 * (double)Math.Sign(MapZeroLatitude) / num) * (180.0 / Math.PI) + 180.0 * Convert.ToDouble(num > 0.0) + MapZeroLongitude; Longitude_deg < -180.0; Longitude_deg += 360.0)
				{
				}
				while (Longitude_deg > 180.0)
				{
					Longitude_deg -= 360.0;
				}
				return;
			}
			double num3 = MapCentreX - x;
			double num4 = (double)(MapCentreY - y) - (double)MapScale / Math.Tan(MapZeroLatitude / (180.0 / Math.PI));
			Longitude_deg = MapZeroLongitude + Math.Atan(num3 / num4) * (180.0 / Math.PI) / Math.Sin(MapZeroLatitude / (180.0 / Math.PI));
			double num5 = (0.0 - Math.Sqrt(num3 * num3 + num4 * num4)) * (double)Math.Sign(MapZeroLatitude);
			Latitude_deg = Math.Atan(num5 / (double)MapScale + 1.0 / Math.Tan(MapZeroLatitude / (180.0 / Math.PI))) * (180.0 / Math.PI) + MapZeroLatitude;
			while (Longitude_deg < -180.0)
			{
				Longitude_deg += 360.0;
			}
			while (Longitude_deg > 180.0)
			{
				Longitude_deg -= 360.0;
			}
		}

		public static void MapPlot(Graphics formGraphics, float xCenter, float yCenter, double WestLongitude_deg, double EastLongitude_deg, double NorthLatitude_deg, double SouthLatitude_deg, bool PlotCities, bool BWflag)
		{
			bool flag = false;
			bool flag2 = false;
			bool flag3 = false;
			ArrayList arrayList = new ArrayList();
			Color color;
			Color color2;
			Color color3;
			Color color4;
			if (BWflag)
			{
				color = Color.Black;
				color2 = Color.Black;
				color3 = Color.Black;
				color4 = Color.Black;
				formGraphics.Clear(Color.White);
			}
			else
			{
				color = Color.Green;
				color2 = Color.Firebrick;
				color3 = Color.SaddleBrown;
				color4 = Color.White;
				formGraphics.Clear(Color.Black);
			}
			Pen pen = new Pen(color4);
			MapCentreX = xCenter;
			MapCentreY = yCenter;
			if (WestLongitude_deg > EastLongitude_deg)
			{
				EastLongitude_deg += 360.0;
			}
			MapZeroLongitude = (WestLongitude_deg + EastLongitude_deg) / 2.0;
			MapZeroLatitude = (NorthLatitude_deg + SouthLatitude_deg) / 2.0;
			if (NorthLatitude_deg == 90.0)
			{
				MapZeroLatitude = 90.0;
			}
			if (SouthLatitude_deg == -90.0)
			{
				MapZeroLatitude = -90.0;
			}
			double num = (EastLongitude_deg - WestLongitude_deg) / 2.0 * Math.Cos(MapZeroLatitude / (180.0 / Math.PI));
			double num2 = (NorthLatitude_deg - SouthLatitude_deg) / 1.4;
			if (Math.Abs(MapZeroLatitude) == 90.0)
			{
				num2 *= 2.4;
			}
			if (num2 > num)
			{
				num = num2;
			}
			MapScale = (float)((double)MapCentreX / num * (180.0 / Math.PI));
			if (MapScale > 100000f)
			{
				MapScale = 100000f;
			}
			if ((double)MapScale / 600.0 < 1.0)
			{
				flag = true;
			}
			double num3 = Math.Round(MapZeroLatitude - 5.0 - num / 2.0);
			double num4 = Math.Round(MapZeroLatitude + 5.0 + num / 2.0);
			double num5 = Math.Round(MapZeroLongitude - 5.0 - num / Math.Cos(MapZeroLatitude / (180.0 / Math.PI)));
			double num6 = Math.Round(MapZeroLongitude + 5.0 + num / Math.Cos(MapZeroLatitude / (180.0 / Math.PI)));
			double num7 = 1.0;
			if (num > 6.0)
			{
				num7 = 2.0;
			}
			if (num > 9.0)
			{
				num7 = 4.0;
			}
			if (Math.Abs(MapZeroLatitude) > 75.0)
			{
				num7 = 5.0;
				if (num3 < -85.0)
				{
					num3 = -90.0;
					num4 = -90.0 + 2.0 * (90.0 + NorthLatitude_deg);
					if (num4 > -10.0)
					{
						num4 = -10.0;
					}
					flag3 = true;
				}
				if (num4 > 85.0)
				{
					num4 = 90.0;
					num3 = 90.0 - 2.0 * (90.0 - SouthLatitude_deg);
					if (num3 < 10.0)
					{
						num3 = 10.0;
					}
					flag2 = true;
				}
				num5 = MapZeroLongitude - 360.0;
				num6 = MapZeroLongitude + 360.0;
				EastLongitude_deg = 360.0;
				WestLongitude_deg = 0.0;
			}
			GraphicsPath graphicsPath = new GraphicsPath();
			pen.Color = color3;
			float x;
			float y;
			for (double num8 = num3; num8 <= num4; num8 += num7)
			{
				arrayList.Clear();
				for (double num9 = num5; num9 <= num6; num9 += num7)
				{
					if (MapProjection(num9, num8, out x, out y) && num9 > num5)
					{
						arrayList.Add(new PointF(x, y));
					}
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					formGraphics.DrawLines(pen, points);
				}
			}
			for (double num9 = num5; num9 <= num6; num9 += num7)
			{
				arrayList.Clear();
				if (MapProjection(num9, num3, out x, out y))
				{
					arrayList.Add(new PointF(x, y));
				}
				if (MapProjection(num9, num4, out x, out y))
				{
					arrayList.Add(new PointF(x, y));
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				if (points.Length > 1)
				{
					formGraphics.DrawLines(pen, points);
				}
			}
			formGraphics.DrawPath(pen, graphicsPath);
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\world.inx", FileMode.Open, FileAccess.Read);
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\World.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			int num10 = (int)(fileStream.Length / 14);
			pen.Color = color;
			pen.DashStyle = DashStyle.Solid;
			for (int i = 1; i < num10; i++)
			{
				if (i > 940)
				{
					pen.Color = color2;
					pen.DashPattern = new float[2] { 2f, 2f };
				}
				int num11 = binaryReader.ReadInt32();
				short num12 = binaryReader.ReadInt16();
				short num13 = binaryReader.ReadInt16();
				short num14 = binaryReader.ReadInt16();
				short num15 = binaryReader.ReadInt16();
				short num16 = binaryReader.ReadInt16();
				int num17 = num11 - 1;
				int num18 = num17 + num12 - 1;
				float num19 = (float)num13 / 90f;
				float num20 = (float)num14 / 90f;
				float num21 = (float)num15 / 90f;
				float num22 = (float)num16 / 90f;
				bool flag4 = false;
				if (flag3)
				{
					flag4 = (double)num21 < num4;
				}
				else if (flag2)
				{
					flag4 = (double)num22 > num3;
				}
				else
				{
					if (num5 < 0.0 && num19 > 180f)
					{
						num19 -= 360f;
					}
					if (num5 < 0.0 && num20 > 180f)
					{
						num20 -= 360f;
					}
					if (num6 > 360.0 && num19 < 180f)
					{
						num19 += 360f;
					}
					if (num6 > 360.0 && num20 < 180f)
					{
						num20 += 360f;
					}
					flag4 = (double)num19 < num6 && (double)num20 > num5 && (double)num21 < num4 && (double)num22 > num3;
				}
				if (!flag4)
				{
					continue;
				}
				arrayList.Clear();
				for (int j = num17; j <= num18; j++)
				{
					fileStream2.Seek(4 * j, SeekOrigin.Begin);
					float num23 = Convert.ToSingle(binaryReader2.ReadInt16());
					float num24 = Convert.ToSingle(binaryReader2.ReadInt16());
					if (MapProjection(num23 / 90f, num24 / 90f, out x, out y))
					{
						arrayList.Add(new PointF(x, y));
					}
					if (flag && j < num18 - 6)
					{
						j += 6;
					}
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					formGraphics.DrawLines(pen, points);
				}
			}
			graphicsPath.Dispose();
			pen.Dispose();
		}

		public static void StarMap(Graphics formGraphics, double RAOfMiddle, double DecOfMiddle, float ChartTopLeft_X, float ChartTopLeft_Y, float ChartSideLength, float ChartHeightDegrees, float MagLimitIn, bool VisualMag, bool GridLines, double RotateAngle, bool FlipHorizontal, bool FlipVertical, bool use_Gaia, bool use_UCAC4, bool use_NOMAD, bool use_PPMXL, bool BWFlag, bool ForAsteroidWorldMap, double CompStarRA, double CompStarDec)
		{
			if (StarMapDrawing)
			{
				return;
			}
			StarMapDrawing = true;
			Gaia.GetAvailableGaiaCatalogues();
			float num = 0f;
			float num2 = 0f;
			float xFromCentre = 0f;
			float yFromCentre = 0f;
			float IntersectX = 0f;
			float IntersectY = 0f;
			string text = "";
			int[] array = new int[4];
			_ = new int[25]
			{
				0, 1, 2, 7, 4, 5, 26, 8, 17, 9,
				11, 12, 13, 14, 24, 15, 16, 18, 19, 20,
				21, 6, 3, 25, 23
			};
			double num3 = 1.0;
			if ((double)ChartHeightDegrees < 3.1)
			{
				num3 = 0.2;
			}
			else if ((double)ChartHeightDegrees < 2.1)
			{
				num3 = 0.5;
			}
			float num4 = 0f;
			Pen pen;
			Brush brush;
			Brush brush3;
			Brush brush4;
			Brush brush5;
			Brush brush6;
			Color color;
			Color color2;
			Color color3;
			if (BWFlag)
			{
				pen = new Pen(Color.Black);
				new Pen(Color.DarkRed);
				brush = Brushes.Plum;
				Brush brush2 = new SolidBrush(Color.Black);
				brush3 = new SolidBrush(Color.Black);
				brush4 = Brushes.White;
				brush5 = Brushes.Black;
				brush6 = Brushes.Black;
				_ = Color.White;
				color = Color.Black;
				_ = Color.Black;
				color2 = Color.Black;
				color3 = Color.Black;
			}
			else
			{
				pen = new Pen(Color.White);
				new Pen(Color.LightBlue);
				brush = Brushes.DarkRed;
				Brush brush2 = new SolidBrush(Color.White);
				brush3 = new SolidBrush(Color.White);
				brush4 = Brushes.Black;
				brush5 = Brushes.Yellow;
				brush6 = Brushes.LightBlue;
				_ = Color.Black;
				color = Color.Blue;
				_ = Color.Gray;
				color2 = Color.Brown;
				color3 = Color.Green;
			}
			Font font = new Font("Courier New", 8f);
			new Font("Symbol", 10f);
			Font font2 = new Font("Courier New", 7f);
			ChartCentre_X = ChartTopLeft_X + ChartSideLength / 2f;
			ChartCentre_Y = ChartTopLeft_Y + ChartSideLength / 2f;
			ChartScale_PixelsPerDeg = ChartSideLength / ChartHeightDegrees;
			Middle_RA = RAOfMiddle;
			Middle_Dec = DecOfMiddle;
			float num5 = ChartSideLength / 2f;
			float num6 = MagLimitIn;
			if (MagLimitIn > 11f)
			{
				num6 = MagLimitIn - 2f;
			}
			if (num6 == 21f)
			{
				if (use_Gaia)
				{
					num6 = 13f;
				}
				else if (use_UCAC4)
				{
					num6 = 18f;
				}
				else if (use_NOMAD || use_PPMXL)
				{
					num6 = 21f;
				}
			}
			double num7 = Middle_RA * (180.0 / Math.PI);
			double num8 = Middle_Dec * (180.0 / Math.PI);
			double num9 = Math.Floor(num8 - 1.2 * (double)ChartHeightDegrees);
			double num10 = Math.Floor(num9 + 2.4 * (double)ChartHeightDegrees + 1.0);
			if (num8 + (double)ChartHeightDegrees > 90.0 || num8 - (double)ChartHeightDegrees < -90.0)
			{
				formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, brush5, ChartCentre_X, ChartCentre_Y - 15f);
				StarMapDrawing = false;
				return;
			}
			float num11;
			float num12;
			if (num8 > 0.0)
			{
				num11 = (float)(1.2 * (double)ChartHeightDegrees / Math.Cos(num10 / (180.0 / Math.PI)));
				num12 = (float)(1.2 * (double)ChartHeightDegrees / Math.Cos(num9 / (180.0 / Math.PI)));
			}
			else
			{
				num11 = (float)(1.2 * (double)ChartHeightDegrees / Math.Cos(num9 / (180.0 / Math.PI)));
				num12 = (float)(1.2 * (double)ChartHeightDegrees / Math.Cos(num10 / (180.0 / Math.PI)));
			}
			if (Math.Abs(num11) > 90f)
			{
				num11 = 90f;
			}
			float num13 = (float)(Math.Floor(num7) - (double)num12);
			float num14 = num13 + 2f * num12 + 4f;
			double num15 = (float)Math.Floor(num13);
			double num16 = (float)Math.Floor(num14);
			pen.Color = color;
			formGraphics.FillRectangle(brush4, ChartTopLeft_X, ChartTopLeft_Y, ChartSideLength, ChartSideLength);
			float x;
			float y;
			if (GridLines)
			{
				pen.Color = color2;
				GraphicsPath graphicsPath = new GraphicsPath();
				int num17 = 1;
				int num18 = 1;
				if ((double)ChartHeightDegrees < 2.1)
				{
					num17 = (num18 = 2);
				}
				if ((double)ChartHeightDegrees < 1.1)
				{
					num17 = (num18 = 3);
				}
				int num19 = -1;
				int num20 = -1;
				num19 = -1;
				for (double num21 = num15 - 1.0; num21 <= num16 + 1.0; num21 += 0.25)
				{
					num19++;
					switch (num19 % 4)
					{
					case 0:
						pen.DashPattern = new float[1] { 1f };
						pen.Color = color2;
						break;
					case 1:
						if (num17 != 3)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 2f, 2f };
						if (BWFlag)
						{
							pen.Color = Color.LightGray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					case 2:
						if (num17 == 1)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 4f, 4f };
						if (BWFlag)
						{
							pen.Color = Color.Gray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					case 3:
						if (num17 != 3)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 2f, 2f };
						if (BWFlag)
						{
							pen.Color = Color.LightGray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					}
					bool flag = false;
					graphicsPath = new GraphicsPath();
					graphicsPath.StartFigure();
					for (double num22 = num9 - 1.0; num22 <= num10 + 1.0; num22 += 0.0625)
					{
						StarMapOrthoProjection(num21 / (180.0 / Math.PI), num22 / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
						float sideX = ChartCentre_X + num5 * (float)Math.Sign(xFromCentre);
						float sideY = ChartCentre_Y + num5 * (float)Math.Sign(yFromCentre);
						bool flag2 = (Math.Abs(xFromCentre) <= num5) & (Math.Abs(yFromCentre) <= num5);
						if (flag && flag2 && num22 > num9 - 1.0)
						{
							graphicsPath.AddLine(num, num2, x, y);
						}
						if (!flag && flag2)
						{
							Intersection(x, y, num, num2, num5, sideX, sideY, out IntersectX, out IntersectY);
							graphicsPath.AddLine(IntersectX, IntersectY, x, y);
						}
						if (flag && !flag2)
						{
							Intersection(num, num2, x, y, num5, sideX, sideY, out IntersectX, out IntersectY);
							graphicsPath.AddLine(num, num2, IntersectX, IntersectY);
						}
						flag = flag2;
						num = x;
						num2 = y;
					}
					if (graphicsPath.PointCount > 1)
					{
						formGraphics.DrawPath(pen, graphicsPath);
					}
				}
				num20 = -1;
				for (double num22 = num9 - 1.0; num22 <= num10 + 1.0; num22 += 0.25)
				{
					num20++;
					switch (num20 % 4)
					{
					case 0:
						pen.DashPattern = new float[1] { 1f };
						pen.Color = color2;
						break;
					case 1:
						if (num18 != 3)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 2f, 2f };
						if (BWFlag)
						{
							pen.Color = Color.LightGray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					case 2:
						if (num18 == 1)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 4f, 4f };
						if (BWFlag)
						{
							pen.Color = Color.Gray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					case 3:
						if (num18 != 3)
						{
							continue;
						}
						pen.DashPattern = new float[2] { 2f, 2f };
						if (BWFlag)
						{
							pen.Color = Color.LightGray;
						}
						else
						{
							pen.Color = Color.DarkRed;
						}
						break;
					}
					bool flag = false;
					graphicsPath = new GraphicsPath();
					graphicsPath.StartFigure();
					for (double num21 = num15 - 1.0; num21 <= num16 + 1.0; num21 += 0.125)
					{
						StarMapOrthoProjection(num21 / (180.0 / Math.PI), num22 / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
						float sideX = ChartCentre_X + num5 * (float)Math.Sign(xFromCentre);
						float sideY = ChartCentre_Y + num5 * (float)Math.Sign(yFromCentre);
						bool flag2 = (Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5);
						if (!(num21 == num15 - 1.0 && flag2))
						{
							if (flag && flag2 && num22 >= num9 - 1.0)
							{
								graphicsPath.AddLine(num, num2, x, y);
							}
							if (!flag && flag2)
							{
								Intersection(x, y, num, num2, num5, sideX, sideY, out IntersectX, out IntersectY);
								graphicsPath.AddLine(IntersectX, IntersectY, x, y);
							}
							if (flag && !flag2)
							{
								Intersection(num, num2, x, y, num5, sideX, sideY, out IntersectX, out IntersectY);
								graphicsPath.AddLine(num, num2, IntersectX, IntersectY);
							}
							Math.Abs(IntersectX);
							_ = 10000f;
						}
						flag = flag2;
						num = x;
						num2 = y;
					}
					if (graphicsPath.PointCount > 1)
					{
						formGraphics.DrawPath(pen, graphicsPath);
					}
				}
				graphicsPath.Dispose();
				GraphicsPath graphicsPath2 = new GraphicsPath();
				pen.DashPattern = new float[2] { 2f, 2f };
				pen.Color = color3;
				if (File.Exists(AppPath + "\\Resource Files\\Constellation Lines EW.dat"))
				{
					StreamReader streamReader = new StreamReader(new FileStream(AppPath + "\\Resource Files\\Constellation Lines EW.dat", FileMode.Open, FileAccess.Read));
					do
					{
						float num23 = Convert.ToSingle(streamReader.ReadLine());
						float num24 = Convert.ToSingle(streamReader.ReadLine());
						float num25 = Convert.ToSingle(streamReader.ReadLine());
						if (!(Math.Abs(num8 - (double)num25) < 1.2 * (double)ChartHeightDegrees))
						{
							continue;
						}
						num23 = 15f * num23;
						num24 = 15f * num24;
						bool flag = false;
						graphicsPath2.StartFigure();
						float num26 = (float)((double)(num24 - num23) / (Math.Floor(num24 - num23) + (double)Convert.ToSingle(Math.Floor(num24 - num23) == 0.0)));
						while ((double)Math.Abs(num26) > num3)
						{
							num26 /= 2f;
						}
						for (float num27 = num23; (double)num27 <= (double)num24 + 0.01; num27 += num26)
						{
							if (!((Math.Abs(num7 - (double)num27) % 360.0 < 90.0) | (Math.Abs(num7 - (double)num27) % 360.0 > 270.0)))
							{
								continue;
							}
							double Dec = (double)num25 / (180.0 / Math.PI);
							double RA = (double)num27 / (180.0 / Math.PI);
							Utilities.PrecessStartToEnd(2405890.0, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec);
							StarMapOrthoProjection(RA, Dec, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
							float sideX = ChartCentre_X + num5 * (float)Math.Sign(xFromCentre);
							float sideY = ChartCentre_Y + num5 * (float)Math.Sign(yFromCentre);
							bool flag2 = (Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5);
							if (!(num27 == num23 && flag2))
							{
								if (flag && flag2)
								{
									graphicsPath2.AddLine(num, num2, x, y);
								}
								if (!flag && flag2)
								{
									Intersection(x, y, num, num2, num5, sideX, sideY, out IntersectX, out IntersectY);
									graphicsPath2.AddLine(IntersectX, IntersectY, x, y);
								}
								if (flag && !flag2)
								{
									Intersection(num, num2, x, y, num5, sideX, sideY, out IntersectX, out IntersectY);
									graphicsPath2.AddLine(num, num2, IntersectX, IntersectY);
								}
							}
							flag = flag2;
							num = x;
							num2 = y;
						}
						formGraphics.DrawPath(pen, graphicsPath2);
					}
					while (!streamReader.EndOfStream);
					streamReader.Close();
				}
				if (File.Exists(AppPath + "\\Resource Files\\Constellation Lines NS.dat"))
				{
					StreamReader streamReader2 = new StreamReader(new FileStream(AppPath + "\\Resource Files\\Constellation Lines NS.dat", FileMode.Open, FileAccess.Read));
					do
					{
						float num28 = Convert.ToSingle(streamReader2.ReadLine());
						float num29 = Convert.ToSingle(streamReader2.ReadLine());
						float num27 = Convert.ToSingle(streamReader2.ReadLine());
						double RA = (double)(15f * num27) / (180.0 / Math.PI);
						double Dec = (double)((num28 + num29) / 2f) / (180.0 / Math.PI);
						Utilities.PrecessStartToEnd(2405890.0, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec);
						if (RA - num7 / (180.0 / Math.PI) > Math.PI)
						{
							RA -= Math.PI * 2.0;
						}
						if (RA - num7 / (180.0 / Math.PI) < -Math.PI)
						{
							RA += Math.PI * 2.0;
						}
						if (!(Math.Abs(num7 - 180.0 / Math.PI * RA) < 1.2 * (double)num11))
						{
							continue;
						}
						if (num8 - (double)num28 < 1.2 * (double)ChartHeightDegrees && (double)num29 - num8 < 1.2 * (double)ChartHeightDegrees)
						{
							bool flag = false;
							graphicsPath2.StartFigure();
							float num30 = (float)(1E-05 + (double)(num28 - num29) / (Math.Floor(num28 - num29) + (double)Convert.ToSingle(Math.Floor(num28 - num29) == 0.0)));
							while ((double)Math.Abs(num30) > num3)
							{
								num30 /= 2f;
							}
							for (double num22 = num29; num22 <= (double)num28 + 0.01; num22 += (double)num30)
							{
								Dec = num22 / (180.0 / Math.PI);
								RA = (double)(15f * num27) / (180.0 / Math.PI);
								Utilities.PrecessStartToEnd(2405890.0, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec);
								StarMapOrthoProjection(RA, Dec, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
								float sideX = ChartCentre_X + num5 * (float)Math.Sign(xFromCentre);
								float sideY = ChartCentre_Y + num5 * (float)Math.Sign(yFromCentre);
								bool flag2 = (Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5);
								if (!(num22 == (double)num29 && flag2))
								{
									if (flag && flag2)
									{
										graphicsPath2.AddLine(num, num2, x, y);
									}
									if (!flag && flag2)
									{
										Intersection(x, y, num, num2, num5, sideX, sideY, out IntersectX, out IntersectY);
										graphicsPath2.AddLine(IntersectX, IntersectY, x, y);
									}
									if (flag && !flag2)
									{
										Intersection(num, num2, x, y, num5, sideX, sideY, out IntersectX, out IntersectY);
										graphicsPath2.AddLine(num, num2, IntersectX, IntersectY);
									}
								}
								flag = flag2;
								num = x;
								num2 = y;
							}
						}
						pen.Color = Color.Green;
						formGraphics.DrawPath(pen, graphicsPath2);
					}
					while (!streamReader2.EndOfStream);
					streamReader2.Close();
				}
				graphicsPath2.Dispose();
				pen.DashStyle = DashStyle.Solid;
				Brush brush2 = Brushes.White;
				int num31 = 0;
				float num32 = 0f;
				float num33 = 0f;
				float num34 = 999999f;
				if (FlipHorizontal)
				{
					num34 = -999999f;
				}
				for (double num21 = num15 - 1.0; num21 <= num16 + 1.0; num21 += 1.0 + Convert.ToDouble(Math.Abs(num8) > 60.0) + Convert.ToDouble(Math.Abs(num8) > 75.0))
				{
					if (!((ChartHeightDegrees > 4f) & (num31 % 2 != 0)))
					{
						double num35 = Math.Floor(num21);
						if (num35 < 0.0)
						{
							num35 += 360.0;
						}
						if (num35 > 359.0)
						{
							num35 -= 360.0;
						}
						string text2 = Utilities.DEGtoDMS(num35 / 15.0, 2, 0, MinutesOnly: true).Trim();
						num32 = formGraphics.MeasureString(text2, font).Width;
						float num36;
						float y2;
						if (num8 > 0.0)
						{
							StarMapOrthoProjection(num21 / (180.0 / Math.PI), (num8 - 0.5 * (double)ChartHeightDegrees) / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
							num36 = (float)((double)x - 0.5 * (double)num32);
							y2 = ChartCentre_Y + num5 - 12f;
						}
						else
						{
							StarMapOrthoProjection(num21 / (180.0 / Math.PI), (num8 + 0.5 * (double)ChartHeightDegrees) / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
							num36 = (float)((double)x - 0.5 * (double)num32);
							y2 = ChartCentre_Y - num5 + 1f;
						}
						if (!FlipHorizontal)
						{
							if ((double)xFromCentre < (double)num5 - 0.5 * (double)num32 && (double)xFromCentre > (double)(0f - num5) + 0.5 * (double)num32 && (double)num36 + 1.2 * (double)num32 < (double)num34)
							{
								num34 = num36;
								formGraphics.DrawString(text2, font, brush6, num36, y2);
							}
						}
						else if ((double)xFromCentre < (double)num5 - 0.5 * (double)num32 && (double)xFromCentre > (double)(0f - num5) + 0.5 * (double)num32 && (double)num34 + 1.2 * (double)num32 < (double)num36)
						{
							num34 = num36;
							formGraphics.DrawString(text2, font, brush6, num36, y2);
						}
					}
					num31++;
				}
				for (double num22 = num9 - 1.0; num22 <= num10 + 1.0; num22 += 1.0)
				{
					if ((ChartHeightDegrees > 4f) & (num22 % 2.0 != 0.0))
					{
						num22 += 1.0;
					}
					string text2 = Convert.ToString(num22);
					num32 = formGraphics.MeasureString(text2, font).Width;
					num33 = formGraphics.MeasureString(text2, font).Height;
					StarMapOrthoProjection((num7 - (double)(ChartHeightDegrees / 2f) / Math.Cos(num22 / (180.0 / Math.PI))) / (180.0 / Math.PI), num22 / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
					float num36 = ChartCentre_X + num5 - num32 - 3f;
					float y2 = (float)((double)y - 0.5 * (double)num33);
					if (yFromCentre < num5 - num33 && yFromCentre > 0f - num5 + num33)
					{
						formGraphics.DrawString(text2, font, brush6, num36, y2);
					}
				}
			}
			if (!GridLines)
			{
				formGraphics.DrawLine(pen, ChartTopLeft_X, ChartCentre_Y, ChartTopLeft_X + (float)(0.1 * (double)ChartSideLength), ChartCentre_Y);
				formGraphics.DrawLine(pen, ChartTopLeft_X + (float)(0.9 * (double)ChartSideLength), ChartCentre_Y, ChartTopLeft_X + ChartSideLength, ChartCentre_Y);
				formGraphics.DrawLine(pen, ChartCentre_X, ChartTopLeft_Y, ChartCentre_X, ChartTopLeft_Y + (float)(0.1 * (double)ChartSideLength));
				formGraphics.DrawLine(pen, ChartCentre_X, ChartTopLeft_Y + (float)(0.9 * (double)ChartSideLength), ChartCentre_X, ChartTopLeft_Y + ChartSideLength);
				pen.Color = Color.Gray;
				formGraphics.DrawEllipse(pen, ChartCentre_X - 10f, ChartCentre_Y - 10f, 20f, 20f);
			}
			if (CompStarDec < 90.0)
			{
				StarMapOrthoProjection(CompStarRA * 15.0 / (180.0 / Math.PI), CompStarDec / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
				if ((Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5))
				{
					formGraphics.FillRectangle(brush, x - 5f, y - 5f, 10f, 10f);
				}
			}
			float num37 = (float)(Middle_RA * (180.0 / Math.PI) - (double)(2f * ChartHeightDegrees) / Math.Cos(Middle_Dec));
			if (num37 < 0f)
			{
				num37 += 360f;
			}
			float num38 = (float)(Middle_RA * (180.0 / Math.PI) + (double)(2f * ChartHeightDegrees) / Math.Cos(Middle_Dec));
			if (num38 > 360f)
			{
				num38 -= 360f;
			}
			float num39 = (float)(Middle_Dec * (180.0 / Math.PI) - (double)ChartHeightDegrees - 0.2);
			float num40 = (float)(Middle_Dec * (180.0 / Math.PI) + (double)ChartHeightDegrees + 0.2);
			if (num40 > 89f)
			{
				num40 = 89f;
			}
			if (num39 < -89f)
			{
				num39 = -89f;
			}
			if (use_Gaia)
			{
				string text3 = AppPath + "\\Resource Files\\Gaia\\";
				string text4 = Gaia.GaiaPrimaryFiles[0];
				if (Gaia.GaiaCatIndex_16_14_12_9[3] && (double)MagLimitIn <= 9.7)
				{
					text4 = Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFilesIndex_16_14_12_9[3]];
				}
				else if (Gaia.GaiaCatIndex_16_14_12_9[2] && (double)MagLimitIn <= 11.7)
				{
					text4 = Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFilesIndex_16_14_12_9[2]];
				}
				else if (Gaia.GaiaCatIndex_16_14_12_9[1] && (double)MagLimitIn <= 13.7)
				{
					text4 = Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFilesIndex_16_14_12_9[1]];
				}
				if (ForAsteroidWorldMap && Settings.Default.AsteroidStarChartEnhanced)
				{
					if (Gaia.GaiaCatIndex_16_14_12_9[3])
					{
						text4 = Gaia.ReservedGaiaFiles[3];
					}
					else if (File.Exists(text3 + "TychoGaia.bin"))
					{
						text4 = "TychoGaia";
					}
					if ((double)MagLimitIn > 11.5 && Gaia.GaiaCatIndex_16_14_12_9[1])
					{
						text4 = Gaia.ReservedGaiaFiles[1];
					}
					if (MagLimitIn < 13f && Gaia.GaiaCatIndex_16_14_12_9[0])
					{
						text4 = Gaia.ReservedGaiaFiles[0];
					}
					if (!File.Exists(text3 + text4 + ".bin"))
					{
						Brush brush2 = Brushes.LightGray;
						formGraphics.DrawString(text4 + "catalogue is not present", font, brush2, ChartCentre_X, ChartCentre_Y);
						StarMapDrawing = false;
						return;
					}
					if (VisualMag)
					{
						text = text4 + " Vmag";
					}
					else
					{
						text = "Gaia Rmag";
					}
				}
				if (!File.Exists(text3 + text4 + ".bin"))
				{
					Brush brush2 = Brushes.LightGray;
					formGraphics.DrawString(text4 + "catalogue is not present", font, brush2, ChartCentre_X, ChartCentre_Y);
					StarMapDrawing = false;
					return;
				}
				text = ((!VisualMag) ? "Gaia Rmag" : (text4 + " Vmag"));
				FileStream fileStream = new FileStream(text3 + text4 + ".inx", FileMode.Open, FileAccess.Read);
				FileStream fileStream2 = new FileStream(text3 + text4 + ".bin", FileMode.Open, FileAccess.Read);
				if (text4.Contains("DR3"))
				{
					Gaia.CurrentRecordLength = 58L;
				}
				else
				{
					Gaia.CurrentRecordLength = 48L;
				}
				BinaryReader binaryReader = new BinaryReader(fileStream);
				BinaryReader binaryReader2 = new BinaryReader(fileStream2);
				int num41 = (int)(179f - 2f * num40);
				if (num40 < 0f)
				{
					num41++;
				}
				if (num41 < 0)
				{
					num41 = 0;
				}
				int num42 = (int)(180f - 2f * num39);
				if (num42 < 0)
				{
					num42 = 0;
				}
				if (num42 > 359)
				{
					num42 = 359;
				}
				for (int i = num41; i <= num42; i++)
				{
					long num43 = 361 * i;
					num15 = Math.Floor((double)num37 - 0.02);
					if (num15 < 0.0)
					{
						num15 += 360.0;
					}
					num16 = Math.Floor((double)num38 + 0.99);
					if (num16 > 360.0)
					{
						num16 -= 360.0;
					}
					fileStream.Seek(num43 * 4, SeekOrigin.Begin);
					array[0] = binaryReader.ReadInt32();
					fileStream.Seek((long)(((double)num43 + num15) * 4.0), SeekOrigin.Begin);
					array[1] = binaryReader.ReadInt32();
					fileStream.Seek((long)(((double)num43 + num16) * 4.0), SeekOrigin.Begin);
					array[2] = binaryReader.ReadInt32();
					fileStream.Seek((num43 + 360) * 4, SeekOrigin.Begin);
					array[3] = binaryReader.ReadInt32();
					int num44 = array[1];
					int num45 = array[2];
					double num46 = 0.0;
					if (num44 >= num45)
					{
						num44 = array[0];
						num45 = array[2];
						num46 = Math.PI * 2.0;
					}
					while (true)
					{
						if (!Gaia.ReadNext(fileStream2, binaryReader2, num44))
						{
							num44++;
						}
						else
						{
							double rA_rad = Gaia.RA_rad;
							double dec_rad = Gaia.Dec_rad;
							float num47 = (float)Gaia.MagGreen;
							float num48 = (float)Gaia.MagRed;
							_ = Gaia.MagBlue;
							float num49;
							if (VisualMag)
							{
								num49 = num47;
								if (num49 > 20f)
								{
									num49 = num48;
								}
							}
							else
							{
								num49 = num48;
								if (num49 > 20f)
								{
									num49 = num47;
								}
							}
							if (num49 <= MagLimitIn)
							{
								if (num49 > num6 - 5f)
								{
									num4 = (num6 - num49) * 9f + 25f;
								}
								if (num49 <= num6 - 5f)
								{
									num4 = (num6 - num49) * 7f + 35f;
								}
								if (num4 < 10f)
								{
									num4 = 10f;
								}
								num4 /= 10f;
								if ((double)num4 < 1.5)
								{
									num4 = 1.5f;
								}
								StarMapOrthoProjection(rA_rad, dec_rad, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
								if ((Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5))
								{
									formGraphics.FillEllipse(brush3, x - num4 / 2f, y - num4 / 2f, num4, num4);
								}
							}
							num44++;
						}
						if (num44 > num45)
						{
							if (num46 == 0.0)
							{
								break;
							}
							num46 = 0.0;
							num44 = array[1];
							num45 = array[3];
							if (!(num46 >= 0.0))
							{
								break;
							}
						}
					}
				}
				binaryReader.Close();
				binaryReader2.Close();
			}
			else if (use_UCAC4)
			{
				if (!File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z900"))
				{
					Brush brush2 = Brushes.LightGray;
					formGraphics.DrawString("UCAC4 catalogue is not present", font, brush2, ChartCentre_X, ChartCentre_Y);
					StarMapDrawing = false;
					return;
				}
				text = ((!VisualMag) ? "UCAC4 red" : "UCAC4 vis");
				UCAC4.InitialiseUCAC4();
				int num50 = (int)(451f + 5f * num40);
				for (int j = (int)(451f + 5f * num39); j <= num50; j++)
				{
					num15 = (int)Math.Floor((double)(num37 * 4f) - 0.02);
					if (num15 < 0.0)
					{
						num15 += 1440.0;
					}
					if (num15 > 1439.0)
					{
						num15 -= 1440.0;
					}
					num16 = (int)Math.Ceiling((double)(num38 * 4f) + 0.02);
					if (num16 < 0.0)
					{
						num16 += 1440.0;
					}
					if (num16 > 1439.0)
					{
						num16 -= 1440.0;
					}
					UCAC4.Open_UCAC4_Catalogue(j);
					array[0] = 0;
					UCAC4.GetUCAC4IndexAndBin(j, (int)num15, out var StartRecordNum, out var NumInBin);
					array[1] = StartRecordNum;
					UCAC4.GetUCAC4IndexAndBin(j, (int)num16, out StartRecordNum, out NumInBin);
					array[2] = StartRecordNum + NumInBin;
					UCAC4.GetUCAC4IndexAndBin(j, 1439, out StartRecordNum, out NumInBin);
					array[3] = StartRecordNum + NumInBin;
					int num51 = array[1];
					int num52 = array[2];
					double num53 = 0.0;
					if (num51 >= num52)
					{
						num51 = array[0];
						num52 = array[2];
						num53 = Math.PI * 2.0;
					}
					while (true)
					{
						UCAC4.Read_UCAC4_entry(num51, UseHipparcosForParallax: false);
						double rA_rad = UCAC4.RA;
						double dec_rad = UCAC4.Dec;
						float num49;
						if (VisualMag)
						{
							num49 = (float)UCAC4.Mag;
							if (num49 >= 30f)
							{
								num49 = (float)UCAC4.MagB;
							}
							if (num49 >= 30f)
							{
								num49 = (float)UCAC4.MagR;
							}
						}
						else
						{
							num49 = (float)UCAC4.MagR;
							if (num49 >= 30f)
							{
								num49 = (float)UCAC4.Mag;
							}
							if (num49 >= 30f)
							{
								num49 = (float)UCAC4.MagB;
							}
						}
						if (num49 <= MagLimitIn)
						{
							if (num49 > num6 - 5f)
							{
								num4 = (num6 - num49) * 9f + 10f;
							}
							if (num49 <= num6 - 5f)
							{
								num4 = (num6 - num49) * 7f + 20f;
							}
							if (num4 < 10f)
							{
								num4 = 10f;
							}
							num4 /= 10f;
							if ((double)num4 < 1.5)
							{
								num4 = 1.5f;
							}
							StarMapOrthoProjection(rA_rad, dec_rad, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
							if ((Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5))
							{
								formGraphics.FillEllipse(brush3, x - num4 / 2f, y - num4 / 2f, num4, num4);
							}
						}
						num51++;
						if (num51 >= num52)
						{
							if (num53 == 0.0)
							{
								break;
							}
							num53 = 0.0;
							num51 = array[1];
							num52 = array[3];
							if (!(num53 >= 0.0))
							{
								break;
							}
						}
					}
					UCAC4.Close_UCAC4_Catalogue();
				}
			}
			else if (use_PPMXL)
			{
				if (!File.Exists(Settings.Default.PPMXL_Path + "\\n89d.dat"))
				{
					Brush brush2 = Brushes.LightGray;
					formGraphics.DrawString("PPMXL catalogue is not present", font, brush2, ChartCentre_X, ChartCentre_Y);
					StarMapDrawing = false;
					return;
				}
				text = ((!VisualMag) ? "PPMX red" : "PPMX blue");
				PPMXL.InitialisePPMXL();
				int num54 = (int)(361f + 4f * num40);
				for (int k = (int)(361f + 4f * num39); k <= num54; k++)
				{
					num15 = (int)Math.Floor((double)(num37 * 5f) - 0.02);
					if (num15 < 0.0)
					{
						num15 += 1800.0;
					}
					if (num15 > 1800.0)
					{
						num15 -= 1800.0;
					}
					num16 = (int)Math.Ceiling((double)(num38 * 5f) + 0.02);
					if (num16 < 0.0)
					{
						num16 += 1800.0;
					}
					if (num16 > 1800.0)
					{
						num16 -= 1800.0;
					}
					PPMXL.Open_PPMXL_Catalogue(k);
					array[0] = 0;
					array[1] = PPMXL.GetPPMXLIndexValue(k, (int)num15);
					array[2] = PPMXL.GetPPMXLIndexValue(k, (int)num16);
					array[3] = PPMXL.GetPPMXLIndexValue(k, 1800);
					int num55 = array[1];
					int num56 = array[2];
					double num57 = 0.0;
					if (num55 >= num56)
					{
						num55 = array[0];
						num56 = array[2];
						num57 = Math.PI * 2.0;
					}
					while (true)
					{
						PPMXL.Read_PPMXL_entry(num55);
						double rA_rad = PPMXL.RA_J2000;
						double dec_rad = PPMXL.Dec_J2000;
						float num49;
						if (VisualMag)
						{
							num49 = (float)PPMXL.mag_V;
							if (num49 >= 25f)
							{
								num49 = (float)PPMXL.mag_B;
							}
							if (num49 >= 25f)
							{
								num49 = (float)PPMXL.mag_R;
							}
						}
						else
						{
							num49 = (float)PPMXL.mag_R;
							if (num49 >= 25f)
							{
								num49 = (float)PPMXL.mag_V;
							}
							if (num49 >= 25f)
							{
								num49 = (float)PPMXL.mag_B;
							}
						}
						if (num49 <= MagLimitIn)
						{
							if (num49 > num6 - 5f)
							{
								num4 = (num6 - num49) * 9f + 10f;
							}
							if (num49 <= num6 - 5f)
							{
								num4 = (num6 - num49) * 7f + 20f;
							}
							if (num4 < 10f)
							{
								num4 = 10f;
							}
							num4 /= 10f;
							if ((double)num4 < 1.5)
							{
								num4 = 1.5f;
							}
							StarMapOrthoProjection(rA_rad, dec_rad, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
							if ((Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5))
							{
								formGraphics.FillEllipse(brush3, x - num4 / 2f, y - num4 / 2f, num4, num4);
							}
						}
						num55++;
						if (num55 >= num56)
						{
							if (num57 == 0.0)
							{
								break;
							}
							num57 = 0.0;
							num55 = array[1];
							num56 = array[3];
							if (!(num57 >= 0.0))
							{
								break;
							}
						}
					}
					PPMXL.Close_PPMXL_Catalogue();
				}
				PPMXL.ReleasePPMXL();
			}
			else if (use_NOMAD)
			{
				string text5 = Settings.Default.NOMAD_Path;
				if (!text5.ToLower().Contains("nomad") | !File.Exists(text5 + "\\090\\m0900.cat"))
				{
					text5 = Settings.Default.NOMAD_Short_path;
				}
				if (!File.Exists(text5 + "\\090\\m0900.cat") & File.Exists(text5 + "\\090\\m0900.inx"))
				{
					Brush brush2 = Brushes.LightGray;
					formGraphics.DrawString("NOMAD catalogue is not present", font, brush2, ChartCentre_X, ChartCentre_Y);
					StarMapDrawing = false;
					return;
				}
				text = ((!VisualMag) ? "NOMAD red" : "NOMAD vis");
				if (NOMAD.InitialiseNOMAD())
				{
					int num58 = (int)(900f + 10f * num40);
					for (int l = (int)(900f + 10f * num39); l <= num58; l++)
					{
						num15 = (int)Math.Floor((double)(num37 * 5f) - 0.02);
						if (num15 < 0.0)
						{
							num15 += 1800.0;
						}
						if (num15 > 1800.0)
						{
							num15 -= 1800.0;
						}
						num16 = (int)Math.Ceiling((double)(num38 * 5f) + 0.02);
						if (num16 < 0.0)
						{
							num16 += 1800.0;
						}
						if (num16 > 1800.0)
						{
							num16 -= 1800.0;
						}
						NOMAD.Open_Nomad_Catalogue_and_Index_Files(l);
						array[0] = 0;
						array[1] = NOMAD.GetNomadIndexValue((int)num15);
						array[2] = NOMAD.GetNomadIndexValue((int)num16);
						array[3] = NOMAD.GetNomadIndexValue(-1);
						int num59 = array[1];
						int num60 = array[2];
						double num61 = 0.0;
						if (num59 >= num60)
						{
							num59 = array[0];
							num60 = array[2];
							num61 = Math.PI * 2.0;
						}
						while (true)
						{
							NOMAD.Read_NOMAD_entry(num59);
							double rA_rad = NOMAD.RA;
							double dec_rad = NOMAD.Dec;
							float num49;
							if (VisualMag)
							{
								num49 = (float)NOMAD.Mv;
								if (num49 >= 30f)
								{
									num49 = (float)NOMAD.Mb;
								}
								if (num49 >= 30f)
								{
									num49 = (float)NOMAD.Mr;
								}
							}
							else
							{
								num49 = (float)NOMAD.Mr;
								if (num49 >= 30f)
								{
									num49 = (float)NOMAD.Mv;
								}
								if (num49 >= 30f)
								{
									num49 = (float)NOMAD.Mb;
								}
							}
							if (num49 <= MagLimitIn)
							{
								if (num49 > num6 - 5f)
								{
									num4 = (num6 - num49) * 9f + 10f;
								}
								if (num49 <= num6 - 5f)
								{
									num4 = (num6 - num49) * 7f + 20f;
								}
								if (num4 < 10f)
								{
									num4 = 10f;
								}
								num4 /= 10f;
								if ((double)num4 < 1.5)
								{
									num4 = 1.5f;
								}
								StarMapOrthoProjection(rA_rad, dec_rad, out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
								if ((Math.Abs(xFromCentre) < num5) & (Math.Abs(yFromCentre) < num5))
								{
									formGraphics.FillEllipse(brush3, x - num4 / 2f, y - num4 / 2f, num4, num4);
								}
							}
							num59++;
							if (num59 >= num60)
							{
								if (num61 == 0.0)
								{
									break;
								}
								num61 = 0.0;
								num59 = array[1];
								num60 = array[3];
								if (!(num61 >= 0.0))
								{
									break;
								}
							}
						}
						NOMAD.Close_Nomad_Catalogue_and_Index_Files();
					}
					NOMAD.ReleaseNOMAD();
				}
			}
			if (GridLines)
			{
				formGraphics.FillRectangle(brush4, ChartTopLeft_X + 10f, ChartTopLeft_Y + ChartSideLength - 25f, formGraphics.MeasureString(text, font2).Width, formGraphics.MeasureString(text, font2).Height);
				formGraphics.DrawString(text, font2, brush5, ChartTopLeft_X + 10f, ChartTopLeft_Y + ChartSideLength - 25f);
				formGraphics.FillRectangle(brush4, ChartTopLeft_X + 10f, ChartTopLeft_Y + ChartSideLength - 15f, formGraphics.MeasureString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2).Width, formGraphics.MeasureString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2).Height);
				formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, brush5, ChartTopLeft_X + 10f, ChartTopLeft_Y + ChartSideLength - 15f);
			}
			Utilities.PopulateStarIDs();
			for (int m = 0; m < Utilities.StarId.Count; m++)
			{
				if (Math.Abs(num7 - Utilities.StarId[m].RAStar) % 360.0 < 90.0 && Math.Abs(num8 - Utilities.StarId[m].DecStar) < 10.0)
				{
					StarMapOrthoProjection(Utilities.StarId[m].RAStar / (180.0 / Math.PI), Utilities.StarId[m].DecStar / (180.0 / Math.PI), out x, out y, out xFromCentre, out yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
					if ((Math.Abs(xFromCentre) < num5 - 8f) & (Math.Abs(yFromCentre) < num5 - 8f))
					{
						formGraphics.DrawString(Utilities.StarId[m].Star_identifier, new Font("Ariel", 9f), brush5, x + 1.5f, y + 1.5f);
					}
				}
			}
			pen.Color = color;
			formGraphics.DrawRectangle(pen, ChartCentre_X - num5, ChartCentre_Y - num5, ChartSideLength - 1f, ChartSideLength - 1f);
			StarMapDrawing = false;
		}

		public static void StarMapOrthoProjection(double RAStar, double DecStar, out float x, out float y, out float xFromCentre, out float yFromCentre, double RotateDeg, bool FlipHorizontal, bool FlipVertical)
		{
			double num = Math.Atan(Math.Tan(DecStar) / Math.Cos(RAStar - Middle_RA));
			xFromCentre = 0f - (float)(180.0 / Math.PI * (double)ChartScale_PixelsPerDeg * Math.Cos(num) * Math.Tan(RAStar - Middle_RA) / Math.Cos(num - Middle_Dec));
			if (FlipHorizontal)
			{
				xFromCentre = 0f - xFromCentre;
			}
			yFromCentre = 0f - (float)(180.0 / Math.PI * (double)ChartScale_PixelsPerDeg * Math.Tan(num - Middle_Dec));
			if (FlipVertical)
			{
				yFromCentre = 0f - yFromCentre;
			}
			Utilities.RotateXY(xFromCentre, yFromCentre, RotateDeg, out var x2, out var y2);
			xFromCentre = (float)x2;
			yFromCentre = (float)y2;
			x = ChartCentre_X + xFromCentre;
			y = ChartCentre_Y + yFromCentre;
		}

		public static void Intersection(float x, float y, float OldX, float OldY, float HalfSide, float SideX, float SideY, out float IntersectX, out float IntersectY)
		{
			float num = x - OldX;
			float num2 = y - OldY;
			float num3 = ((num != 0f) ? ((SideX - OldX) / num) : 0.001f);
			float num4 = ((num2 != 0f) ? ((SideY - OldY) / num2) : 0.001f);
			float num5;
			if (num3 > 1f || num3 < 0f)
			{
				num5 = num4;
			}
			else if (num4 > 1f || num4 < 0f)
			{
				num5 = num3;
			}
			else
			{
				IntersectY = OldY + num3 * num2;
				num5 = ((!(Math.Abs(IntersectY - ChartCentre_Y) < HalfSide)) ? num4 : num3);
			}
			IntersectX = OldX + num5 * num;
			IntersectY = OldY + num5 * num2;
		}
	}
}
