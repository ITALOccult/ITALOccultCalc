using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Eclipses
{
	public class CraterReductions : Form
	{
		internal static List<CraterCoords> Craters = new List<CraterCoords>();

		internal static CraterCoords Cratercoords;

		private const double Radian = 180.0 / Math.PI;

		private const double MoonRadiusKm = 1737.4;

		private const double MoonRadius = 0.27239929151725656;

		private static int ColumnOffset = 0;

		private static double[] MCrisiumLongitude = new double[53]
		{
			50.0, 51.0, 52.0, 53.0, 54.0, 54.0, 55.0, 56.0, 57.0, 58.0,
			59.0, 60.0, 61.0, 62.0, 63.0, 64.0, 65.0, 65.0, 66.0, 67.0,
			68.0, 69.0, 69.3, 69.0, 68.5, 69.4, 69.0, 68.2, 68.0, 67.0,
			66.0, 66.0, 65.0, 64.0, 63.4, 63.0, 62.0, 61.0, 60.0, 59.0,
			58.0, 57.0, 56.0, 55.0, 54.0, 53.0, 52.0, 51.0, 50.0, 50.2,
			50.6, 49.6, 49.8
		};

		private static double[] MCrisiumLatitude = new double[53]
		{
			14.3, 13.5, 12.5, 11.7, 11.8, 10.8, 10.5, 9.8, 9.3, 9.9,
			10.5, 10.1, 10.7, 10.6, 11.4, 12.3, 15.0, 14.1, 14.5, 14.3,
			16.5, 13.6, 15.0, 15.8, 17.0, 18.0, 19.0, 19.6, 19.5, 20.2,
			20.5, 21.1, 21.5, 21.6, 21.8, 23.8, 24.3, 24.6, 24.0, 23.9,
			23.5, 23.1, 23.0, 22.7, 22.3, 22.0, 21.1, 19.5, 18.0, 17.0,
			16.0, 16.0, 15.0
		};

		private static int CrisiumCount = MCrisiumLongitude.GetUpperBound(0);

		private IContainer components;

		private Button cmdReduce;

		private ProgressBar pBar;

		private Button cmdHelp;

		public CraterReductions()
		{
			InitializeComponent();
			CreateCraterList();
		}

		private void cmdReduce_Click(object sender, EventArgs e)
		{
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0120: Expected O, but got Unknown
			//IL_015f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Invalid comparison between Unknown and I4
			string text = "";
			string text2 = "+";
			string text3 = "+";
			string text4 = "+";
			int num = 0;
			int num2 = 0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			double Clong = 0.0;
			double Clat = 0.0;
			double Cdia = 0.0;
			double num13 = 0.0;
			double num14 = 0.0;
			double num15 = 0.0;
			double num16 = Math.PI / 720.0;
			double lSun_deg = 0.0;
			double bSun_deg = 0.0;
			double CSun_deg = 0.0;
			bool flag = false;
			pBar.set_Value(num2);
			((Control)pBar).set_Visible(true);
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Open Crater Timing file");
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.CraterTimingFile));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(Path.GetFileName(Settings.Default.CraterTimingFile));
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.CraterTimingFile = ((FileDialog)val).get_FileName();
				flag = new FileInfo(((FileDialog)val).get_FileName()).Length < 10000;
				string path;
				do
				{
					path = Path.GetDirectoryName(((FileDialog)val).get_FileName()) + "/" + Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName()) + "_v" + num + ".txt";
					if (!File.Exists(path))
					{
						break;
					}
					num++;
				}
				while (num < 1000);
				using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName());
				using StreamWriter streamWriter = new StreamWriter(path);
				do
				{
					num2++;
					if (num2 % 100 == 0)
					{
						try
						{
							pBar.set_Value(num2);
							Application.DoEvents();
						}
						catch
						{
						}
					}
					string text5 = streamReader.ReadLine();
					CraterEvents.DecodeLine(text5);
					string text6 = CraterEvents.Crater.ToUpper();
					double jD_UT = CraterEvents.JD_UT;
					double dT = CraterEvents.dT;
					bool flag2 = (CraterEvents.C_event == 0) | (CraterEvents.C_event == 4) | (CraterEvents.C_event == 5) | (CraterEvents.C_event == 9);
					Utilities.GeocentricMoon(jD_UT, out var RA, out var Dec, out var PiMoon, out var l, out var b, out var C);
					Utilities.PlanetGeocentric(jD_UT + dT, 3, 1E-06, 0, out var RA2, out var Dec2, out var GeocentricDist, out var _, out var Diameter);
					RA2 += Math.PI;
					Dec2 = 0.0 - Dec2;
					double num17 = Math.Cos(Dec) * Math.Sin(RA - RA2) / Math.Sin(PiMoon);
					double num18 = (Math.Sin(Dec) * Math.Cos(Dec2) - Math.Cos(Dec) * Math.Sin(Dec2) * Math.Cos(RA - RA2)) / Math.Sin(PiMoon);
					double num19 = (Math.Sin(Dec) * Math.Sin(Dec2) + Math.Cos(Dec) * Math.Cos(Dec2) * Math.Cos(RA - RA2)) / Math.Sin(PiMoon);
					num8 = (num9 = (num10 = 0.0));
					if (flag2)
					{
						double num20 = Math.Atan2(num17, num18);
						if (text5.Substring(ColumnOffset + 11, 20).Trim().ToUpper()
							.Contains("FIRST") | text5.Substring(ColumnOffset + 11, 20).Trim().ToUpper()
							.Contains("FOURTH"))
						{
							num17 -= 0.27239929151725656 * Math.Sin(num20);
							num18 -= 0.27239929151725656 * Math.Cos(num20);
						}
						else
						{
							num17 += 0.27239929151725656 * Math.Sin(num20);
							num18 += 0.27239929151725656 * Math.Cos(num20);
						}
						num5 = 0.0;
						lSun_deg = (bSun_deg = (CSun_deg = 0.0));
					}
					else
					{
						if (text != text6)
						{
							GetCraterCoords(text6, out Clong, out Clat, out Cdia, out var valid);
							if (!valid)
							{
								continue;
							}
							text = text6;
						}
						Utilities.Librations(jD_UT + dT, RA, Dec, PiMoon, GetEarthSelenographic: false, GetSunSelenographic: true, RA2 - Math.PI, 0.0 - Dec2, GeocentricDist, out l, out b, out C, out lSun_deg, out bSun_deg, out CSun_deg, out var _);
						num13 = lSun_deg / (180.0 / Math.PI);
						num14 = bSun_deg / (180.0 / Math.PI);
						C = CSun_deg / (180.0 / Math.PI);
						if (text6.Contains("CRISIUM") & ((CraterEvents.C_event != 2) & (CraterEvents.C_event != 7)))
						{
							bool flag3 = true;
							if ((CraterEvents.C_event == 3) | (CraterEvents.C_event == 6))
							{
								flag3 = false;
							}
							num11 = 1.0;
							num12 = 0.0;
							for (int i = 0; i < CrisiumCount; i++)
							{
								num3 = 0.27239929151725656 * Math.Cos(MCrisiumLatitude[i] / (180.0 / Math.PI)) * Math.Sin(MCrisiumLongitude[i] / (180.0 / Math.PI) - num13);
								num4 = 0.27239929151725656 * (Math.Sin(MCrisiumLatitude[i] / (180.0 / Math.PI)) * Math.Cos(num14) - Math.Cos(MCrisiumLatitude[i] / (180.0 / Math.PI)) * Math.Sin(num14) * Math.Cos(MCrisiumLongitude[i] / (180.0 / Math.PI) - num13));
								num5 = 0.27239929151725656 * (Math.Sin(MCrisiumLatitude[i] / (180.0 / Math.PI)) * Math.Sin(num14) + Math.Cos(MCrisiumLatitude[i] / (180.0 / Math.PI)) * Math.Cos(num14) * Math.Cos(MCrisiumLongitude[i] / (180.0 / Math.PI) - num13));
								num6 = 0.0 - (num3 * Math.Cos(C) - num4 * Math.Sin(C));
								num7 = num4 * Math.Cos(C) + num3 * Math.Sin(C);
								double num21 = num17 + num6;
								double num22 = num18 + num7;
								double num23 = Math.Sqrt(num21 * num21 + num22 * num22);
								if (flag3)
								{
									if (num23 < num11)
									{
										num11 = num23;
										num8 = num6;
										num9 = num7;
									}
								}
								else if (num23 > num12)
								{
									num12 = num23;
									num8 = num6;
									num9 = num7;
								}
							}
							num17 += num8;
							num18 += num9;
						}
						else
						{
							num3 = 0.27239929151725656 * Math.Cos(Clat) * Math.Sin(Clong - num13);
							num4 = 0.27239929151725656 * (Math.Sin(Clat) * Math.Cos(num14) - Math.Cos(Clat) * Math.Sin(num14) * Math.Cos(Clong - num13));
							num5 = 0.27239929151725656 * (Math.Sin(Clat) * Math.Sin(num14) + Math.Cos(Clat) * Math.Cos(num14) * Math.Cos(Clong - num13));
							num6 = 0.0 - (num3 * Math.Cos(C) - num4 * Math.Sin(C));
							num7 = num4 * Math.Cos(C) + num3 * Math.Sin(C);
							num17 += num6;
							num18 += num7;
							if ((CraterEvents.C_event == 1) | (CraterEvents.C_event == 3) | (CraterEvents.C_event == 6) | (CraterEvents.C_event == 8))
							{
								GetCraterRimOffsetSpherical(num6, num7, num5, Cdia, num17, num18, CraterEvents.C_event, out num8, out num9, out num10);
								num17 += num8;
								num18 += num9;
								num5 += num10;
							}
						}
					}
					double num24 = Math.Sqrt(num17 * num17 + num18 * num18);
					num15 = Math.Atan(num18 / Math.Abs(num17));
					num16 = (Diameter - 8.794 / GeocentricDist) / 3600.0 / (180.0 / Math.PI);
					double earthRadius = 1.0 - 0.00335 * Math.Pow(Math.Cos(Dec2 + Math.Sin(num15) * num16) * Math.Sin(num15), 2.0);
					ShadowRadius(Diameter / 3600.0 / (180.0 / Math.PI), 8.794 / GeocentricDist / 3600.0 / (180.0 / Math.PI), earthRadius, num19 - num5, out var UmbralRadiusAt0km, out var UmbralRadiusAt150km);
					double num25 = (num24 - UmbralRadiusAt0km) / (UmbralRadiusAt150km - UmbralRadiusAt0km) * 150.0;
					double num26 = 100.0 * (num24 / UmbralRadiusAt0km - 1.0);
					StringBuilder stringBuilder = new StringBuilder();
					text3 = "+";
					if (num25 < 0.0)
					{
						text3 = "-";
					}
					stringBuilder.Append((text3 + string.Format("{0,3:f0} ", Math.Abs(num25)).TrimStart(Array.Empty<char>())).PadLeft(5));
					text2 = "+";
					if (num15 < 0.0)
					{
						text2 = "-";
					}
					stringBuilder.Append((text2 + string.Format("{0,2:f0} ", Math.Abs(num15 * (180.0 / Math.PI))).TrimStart(Array.Empty<char>())).PadLeft(4));
					text4 = "+";
					if (num26 < 0.0)
					{
						text4 = "-";
					}
					stringBuilder.AppendFormat(text4 + string.Format("{0,3:f1} ", Math.Abs(num26)));
					streamWriter.WriteLine(text5.Substring(0, 46) + stringBuilder.ToString() + text5.Substring(60));
					if (flag)
					{
						streamWriter.WriteLine("Umbral Dist (obsd) = {0,9:f5}", num24);
						streamWriter.WriteLine("Danjon at 0km      = {0,9:f5}", UmbralRadiusAt0km);
						streamWriter.WriteLine("Danjon at 150km    = {0,9:f5}", UmbralRadiusAt150km);
						streamWriter.WriteLine("Danjon height (km) = {0,9:f1}", num25);
						streamWriter.WriteLine();
					}
				}
				while (!streamReader.EndOfStream);
			}
			((Control)pBar).set_Visible(false);
		}

		internal static void GetCraterRimOffsetSpherical(double X_CraterCentre, double Y_CraterCentre, double Z_CraterCentre, double C_Radius_km, double X_CraterCenterInUmbra, double Y_CraterCenterInUmbra, int CraterEvent, out double Xrim, out double Yrim, out double Zrim)
		{
			Xrim = (Yrim = (Zrim = 0.0));
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = Math.Atan(X_CraterCentre / Z_CraterCentre);
			double num5 = Math.Atan(Y_CraterCentre / Math.Sqrt(X_CraterCentre * X_CraterCentre + Z_CraterCentre * Z_CraterCentre));
			double num6 = C_Radius_km / 1737.4;
			double num7 = Math.Sin(num6);
			double num8 = Math.Cos(num6);
			double num9 = Math.Sin(Math.PI / 2.0 - num5);
			double num10 = Math.Cos(Math.PI / 2.0 - num5);
			bool flag = true;
			if (CraterEvent == 3 || CraterEvent == 6)
			{
				flag = false;
			}
			double num11 = 10.0;
			double num12 = 0.0;
			double num13 = 1.0;
			if (C_Radius_km < 50.0)
			{
				num13 = 2.0;
			}
			if (C_Radius_km < 25.0)
			{
				num13 = 4.0;
			}
			if (C_Radius_km < 10.0)
			{
				num13 = 8.0;
			}
			if (C_Radius_km < 6.0)
			{
				num13 = 15.0;
			}
			num13 /= 2.0;
			for (double num14 = 0.0; num14 < 360.0; num14 += num13)
			{
				double num15 = num14 / (180.0 / Math.PI);
				double num16 = num7 * Math.Sin(num15);
				double num17 = num8 * num9 + num7 * num10 * Math.Cos(num15);
				double x = (0.0 - num8) * num10 + num7 * num9 * Math.Cos(num15);
				num = Math.Atan(num16 / num17);
				num2 = Math.Atan2(Math.Sqrt(num16 * num16 + num17 * num17), x);
				if (num2 > Math.PI)
				{
					num2 = Math.PI - num2;
				}
				num3 = num2 - Math.PI / 2.0;
				double num18 = 0.27239929151725656 * Math.Cos(num3) * Math.Sin(num4 - num) - X_CraterCentre;
				double num19 = 0.27239929151725656 * Math.Sin(num3) - Y_CraterCentre;
				double num20 = 0.27239929151725656 * Math.Cos(num3) * Math.Cos(num4 - num) - Z_CraterCentre;
				double num21 = X_CraterCenterInUmbra + num18;
				double num22 = Y_CraterCenterInUmbra + num19;
				double num23 = Math.Sqrt(num21 * num21 + num22 * num22);
				if (flag)
				{
					if (num23 < num11)
					{
						num11 = num23;
						Xrim = num18;
						Yrim = num19;
						Zrim = num20;
					}
				}
				else if (num23 > num12)
				{
					num12 = num23;
					Xrim = num18;
					Yrim = num19;
					Zrim = num20;
				}
			}
		}

		internal static void ShadowRadius(double SolarRadiusRadian, double SolarParallaxRadian, double EarthRadius, double ZDistance, out double UmbralRadiusAt0km, out double UmbralRadiusAt150km)
		{
			double num = SolarRadiusRadian - SolarParallaxRadian * EarthRadius;
			double num2 = EarthRadius / Math.Sin(num);
			UmbralRadiusAt0km = EarthRadius / Math.Cos(num) * (1.0 - ZDistance / num2);
			num = SolarRadiusRadian - SolarParallaxRadian * (EarthRadius + 0.02351783914331097);
			num2 = (EarthRadius + 0.02351783914331097) / Math.Sin(num);
			UmbralRadiusAt150km = (EarthRadius + 0.02351783914331097) / Math.Cos(num) * (1.0 - ZDistance / num2);
		}

		internal static bool CreateCraterList()
		{
			Craters.Clear();
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\CraterTimings.bin", FileMode.Open, FileAccess.Read))
			{
				if (fileStream.Length < 10000)
				{
					return false;
				}
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				for (int i = 0; i < fileStream.Length / 28; i++)
				{
					Cratercoords = new CraterCoords();
					Cratercoords.Lon = (double)binaryReader.ReadInt16() / 100.0;
					Cratercoords.Lat = (double)binaryReader.ReadInt16() / 100.0;
					Cratercoords.RadiusKm = (double)binaryReader.ReadInt32() / 200.0;
					Cratercoords.Name = new string(binaryReader.ReadChars(20));
					Craters.Add(Cratercoords);
				}
				Craters.Sort();
			}
			return true;
		}

		internal static void GetCraterCoords(string Name, out double Clong, out double Clat, out double Cdia, out bool valid)
		{
			Clong = (Clat = (Cdia = 0.0));
			valid = false;
			int num = 0;
			int num2 = Craters.Count - 1;
			string strA = Name.Trim().ToUpper();
			do
			{
				int num3 = (num2 + num) / 2;
				int num4 = string.Compare(strA, Craters[num3].Name.Trim().ToUpper(), ignoreCase: true);
				if (num4 == 0)
				{
					Clong = Craters[num3].Lon / (180.0 / Math.PI);
					Clat = Craters[num3].Lat / (180.0 / Math.PI);
					Cdia = Craters[num3].RadiusKm;
					if ((Clong < 100.0) & (Clat < 95.0))
					{
						valid = true;
					}
					break;
				}
				if (num4 < 0)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"CraterTimings");
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			cmdReduce = new Button();
			pBar = new ProgressBar();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)cmdReduce).set_Location(new Point(62, 10));
			((Control)cmdReduce).set_Name("cmdReduce");
			((Control)cmdReduce).set_Size(new Size(67, 45));
			((Control)cmdReduce).set_TabIndex(0);
			((Control)cmdReduce).set_Text("Reduce");
			((ButtonBase)cmdReduce).set_UseVisualStyleBackColor(true);
			((Control)cmdReduce).add_Click((EventHandler)cmdReduce_Click);
			((Control)pBar).set_Location(new Point(30, 60));
			pBar.set_Maximum(23000);
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(178, 10));
			((Control)pBar).set_TabIndex(1);
			((Control)pBar).set_Visible(false);
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(188, 12));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(42, 40));
			((Control)cmdHelp).set_TabIndex(3);
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(234, 75));
			((Control)this).get_Controls().Add((Control)(object)pBar);
			((Control)this).get_Controls().Add((Control)(object)cmdReduce);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("CraterReductions");
			((Control)this).set_Text("Crater-timing Reductions");
			((Control)this).ResumeLayout(false);
		}
	}
}
