using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using Occult.Properties;

namespace Occult
{
	public static class DoubleStars
	{
		internal static ArrayList XZDoubleList = new ArrayList();

		internal static List<DoubleStarLine> XZOrbitsList = new List<DoubleStarLine>();

		private static List<DoubleStarLine> XZDoub = new List<DoubleStarLine>();

		private static List<DoubleStarSepPA> XZSepPa = new List<DoubleStarSepPA>();

		public static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		internal static int LastXZforMean = 0;

		internal static string LastComponentForMean = " ";

		internal static double DoubleStarMinSep = (double)Settings.Default.DoubleStarMinSep;

		internal static double DoubleStarMaxSep = (double)Settings.Default.DoubleStarMaxSep;

		internal static void GetXZDoubleMatches(int XZ, double JD, bool ForReductions, out int Count, string[] Components, double[] PosnAngle, double[] Separation, bool[] IsMean, out string ComponentIDs, out string DiscovererName, out bool NonInstantaneous, out bool ObservationsWanted)
		{
			double year = Utilities.BesselianYear(JD);
			_ = new char[200];
			XZDoub.Clear();
			XZSepPa.Clear();
			NonInstantaneous = false;
			ObservationsWanted = false;
			for (int i = 0; i < 4; i++)
			{
				PosnAngle[i] = 0.0;
				Separation[i] = 0.0;
				IsMean[i] = false;
				Components[i] = "";
			}
			XZ80Q.Get_XZ_Star(XZ);
			string text = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true);
			text = (text.Substring(0, 2) + text.Substring(3, 2) + text.Substring(6, 1)).Replace(' ', '0');
			int num = int.Parse(text) - 2;
			if (num < 0)
			{
				num = 0;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZDoubles.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num2 = (int)(fileStream.Length / 200);
			int num3 = num2 - 1;
			int num4 = 0;
			int num5;
			do
			{
				num5 = (num3 + num4) / 2;
				fileStream.Seek((long)num5 * 200L, SeekOrigin.Begin);
				int num6 = int.Parse(new string(binaryReader.ReadChars(200), 21, 5));
				if (num == num6)
				{
					break;
				}
				if (num < num6)
				{
					num3 = num5 - 1;
				}
				else
				{
					num4 = num5 + 1;
				}
			}
			while (num4 <= num3);
			int num7 = num5;
			if (num7 < 0)
			{
				num7 = 0;
			}
			do
			{
				fileStream.Seek((long)num7 * 200L, SeekOrigin.Begin);
				string d = new string(binaryReader.ReadChars(200));
				DoubleStarLine doubleStarLine = new DoubleStarLine();
				doubleStarLine.ReadLine(d);
				if (((XZ == doubleStarLine.XZa) | (XZ == doubleStarLine.XZb)) && !(doubleStarLine.ID.Substring(0, 3) == "OC*"))
				{
					if (doubleStarLine.ID.Substring(0, 3) == "OCc")
					{
						NonInstantaneous = true;
					}
					if (doubleStarLine.ID.Substring(0, 3) == "GC ")
					{
						NonInstantaneous = true;
					}
					if (doubleStarLine.ID.Substring(0, 3) == "S  ")
					{
						NonInstantaneous = true;
					}
					if (doubleStarLine.ID.Substring(0, 3) == "   ")
					{
						NonInstantaneous = true;
					}
					if (doubleStarLine.ID.Substring(0, 3) == "---")
					{
						NonInstantaneous = true;
					}
					string text2 = doubleStarLine.Component.Trim();
					if (text2.Length < 1 && doubleStarLine.ID.Substring(0, 3).ToUpper() != "OCC")
					{
						text2 = "AB";
					}
					XZDoub.Add(doubleStarLine);
					DoubleStarSepPA doubleStarSepPA = new DoubleStarSepPA();
					bool flag = true;
					if (XZ == doubleStarLine.XZb)
					{
						flag = false;
					}
					doubleStarLine.PAandSep(year, out var PA, out var Sep);
					doubleStarSepPA.Sep = Sep;
					if ((Sep < DoubleStarMaxSep) & (Sep > DoubleStarMinSep))
					{
						ObservationsWanted = true;
					}
					if (doubleStarLine.ID.Substring(0, 3).ToUpper() == "CHR")
					{
						ObservationsWanted = true;
					}
					doubleStarSepPA.MeanRatio = 1.0;
					if (flag)
					{
						doubleStarSepPA.WDS_ID = text2;
						doubleStarSepPA.PA = PA;
						doubleStarSepPA.MagA = doubleStarLine.MA;
						doubleStarSepPA.MagB = doubleStarLine.MB;
						if (doubleStarLine.IsMean)
						{
							doubleStarSepPA.MeanRatio = MeanPositionRatio(doubleStarLine.MA, doubleStarLine.MB);
						}
					}
					else
					{
						doubleStarSepPA.WDS_ID = ReversedWDS(text2);
						if (PA < 180.0)
						{
							doubleStarSepPA.PA = PA + 180.0;
						}
						else
						{
							doubleStarSepPA.PA = PA - 180.0;
						}
						doubleStarSepPA.MagA = doubleStarLine.MB;
						doubleStarSepPA.MagB = doubleStarLine.MA;
					}
					doubleStarSepPA.WDS_Name = doubleStarLine.ID;
					XZSepPa.Add(doubleStarSepPA);
				}
				if (doubleStarLine.RAasNumber - num > 4)
				{
					break;
				}
				num7++;
			}
			while (num7 < num2);
			fileStream.Close();
			XZSepPa.Sort();
			Count = XZSepPa.Count;
			for (int j = 0; j < 4; j++)
			{
				if (Count > j)
				{
					if (ForReductions)
					{
						Components[j] = XZSepPa[j].ReductionString();
					}
					else
					{
						Components[j] = XZSepPa[j].ToString();
					}
					PosnAngle[j] = XZSepPa[j].PA;
					Separation[j] = XZSepPa[j].Sep;
					IsMean[j] = XZSepPa[j].MeanRatio != 1.0;
				}
				else if (ForReductions)
				{
					Components[j] = "      ";
				}
			}
			ComponentIDs = "";
			for (int k = 0; k < Count && k <= 3; k++)
			{
				string text3 = Components[k].Substring(0, 5).Trim();
				int num8 = text3.IndexOf(" ");
				if (num8 > 0)
				{
					text3 = text3.Substring(0, num8);
				}
				for (int l = 0; l < text3.Length; l++)
				{
					string text4 = text3.Substring(l, 1);
					if (!ComponentIDs.Contains(text4))
					{
						ComponentIDs += text4;
					}
				}
			}
			DiscovererName = "";
			for (int m = 0; m < Count; m++)
			{
				DiscovererName += XZSepPa[m].WDS_Name;
			}
		}

		internal static double MeanPositionRatio(double M1, double M2)
		{
			if (M1 < -5.0)
			{
				return 0.0;
			}
			if (M2 < -5.0)
			{
				return 1.0;
			}
			double num = Math.Pow(10.0, M1 / -2.5);
			double num2 = Math.Pow(10.0, M2 / -2.5);
			double num3 = num + num2;
			return num / num3;
		}

		internal static string ReversedWDS(string WDS)
		{
			if (WDS.Length < 2)
			{
				return WDS;
			}
			if (WDS.Length == 2)
			{
				return WDS.Substring(1, 1) + WDS.Substring(0, 1);
			}
			int num = WDS.IndexOf("-");
			if (num > 0)
			{
				return WDS.Substring(num + 1) + "-" + WDS.Substring(0, num);
			}
			num = WDS.IndexOf(",");
			if (num > 0)
			{
				return WDS.Substring(num + 1) + "," + WDS.Substring(0, num);
			}
			return WDS;
		}

		internal static void GetXZDoubleList(int XZ)
		{
			int num = 0;
			int num2 = 0;
			_ = new char[200];
			XZDoubleList.Clear();
			XZOrbitsList.Clear();
			XZ80Q.Get_XZ_Star(XZ);
			string text = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true);
			text = (text.Substring(0, 2) + text.Substring(3, 2) + text.Substring(6, 1)).Replace(' ', '0');
			int num3 = int.Parse(text) - 2;
			if (num3 < 0)
			{
				num3 = 0;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZDoubles.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num4 = (int)(fileStream.Length / 200);
			int num5 = num4 - 1;
			int num6 = 0;
			int num7;
			do
			{
				num7 = (num5 + num6) / 2;
				fileStream.Seek((long)num7 * 200L, SeekOrigin.Begin);
				int num8 = int.Parse(new string(binaryReader.ReadChars(200), 21, 5));
				if (num3 == num8)
				{
					break;
				}
				if (num3 < num8)
				{
					num5 = num7 - 1;
				}
				else
				{
					num6 = num7 + 1;
				}
			}
			while (num6 <= num5);
			int num9 = num7;
			if (num9 < 0)
			{
				num9 = 0;
			}
			do
			{
				fileStream.Seek((long)num9 * 200L, SeekOrigin.Begin);
				string d = new string(binaryReader.ReadChars(200));
				DoubleStarLine doubleStarLine = new DoubleStarLine();
				doubleStarLine.ReadLine(d);
				if ((XZ == doubleStarLine.XZa) | (XZ == doubleStarLine.XZb))
				{
					_ = doubleStarLine.XZmain;
					num = num9 - 10;
					if (num < 0)
					{
						num = 0;
					}
					num2 = num9 + 20;
					if (num2 > num4 - 1)
					{
						num2 = num4 - 1;
					}
					XZDoubleList.Clear();
					string text2 = doubleStarLine.Component.Trim();
					if (doubleStarLine.IsMean & (XZ == doubleStarLine.XZa))
					{
						if (text2.Length > 0)
						{
							XZDoubleList.Add("Star is the mean position of " + doubleStarLine.Component.Trim());
						}
						else
						{
							XZDoubleList.Add("Star is the mean position of the following pair");
						}
					}
					else if (XZ == doubleStarLine.XZa)
					{
						if (text2.Length > 0)
						{
							XZDoubleList.Add("Star is component " + doubleStarLine.Component.Substring(0, 1) + " in the following system");
						}
						else
						{
							XZDoubleList.Add("Star is the primary of the following pair");
						}
					}
					else if (text2.Length > 0)
					{
						XZDoubleList.Add("Star is component " + text2.Substring(text2.Length - 1, 1) + " in the following system");
					}
					else
					{
						XZDoubleList.Add("Star is the secondary of the following pair");
					}
					break;
				}
				if (doubleStarLine.RAasNumber - num3 > 4)
				{
					break;
				}
				num9++;
			}
			while (num9 < num4);
			for (num9 = num; num9 <= num2; num9++)
			{
				fileStream.Seek((long)num9 * 200L, SeekOrigin.Begin);
				string d = new string(binaryReader.ReadChars(200));
				DoubleStarLine doubleStarLine = new DoubleStarLine();
				doubleStarLine.ReadLine(d);
				if ((XZ == doubleStarLine.XZa) | (XZ == doubleStarLine.XZb) | (XZ == doubleStarLine.XZmain))
				{
					XZDoubleList.Add(doubleStarLine.DoubleMainText());
					if (doubleStarLine.SemiMajor > 0.0)
					{
						XZOrbitsList.Add(doubleStarLine);
					}
				}
			}
			fileStream.Close();
		}

		internal static string WDSName(string Nam)
		{
			char[] array = new char[40];
			string result;
			if (Nam == "OCc")
			{
				result = "OCc  Possible occultation discovery";
			}
			else if (Nam == "OC*")
			{
				result = "OC*  Star is not a double";
			}
			else
			{
				FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\WDS Discovery Codes.dat", FileMode.Open, FileAccess.Read);
				BinaryReader binaryReader = new BinaryReader(fileStream);
				int num = 0;
				int num2 = (int)fileStream.Length / 42 - 1;
				result = "";
				do
				{
					int num3 = (num + num2) / 2;
					fileStream.Seek(num3 * 42, SeekOrigin.Begin);
					array = binaryReader.ReadChars(42);
					string text = new string(array, 0, 3);
					if (text == Nam)
					{
						string text2 = new string(array, 4, 36);
						result = text + "  " + text2;
						break;
					}
					if (text.CompareTo(Nam) > 0)
					{
						num2 = num3 - 1;
					}
					else
					{
						num = num3 + 1;
					}
				}
				while (num2 >= num);
				fileStream.Close();
			}
			return result;
		}

		internal static string ComponentIfMeanPosition(int XZ)
		{
			if (LastXZforMean == XZ && XZ > 0)
			{
				return LastComponentForMean;
			}
			string text = " ";
			_ = new char[200];
			XZDoub.Clear();
			XZ80Q.Get_XZ_Star(XZ);
			string text2 = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true);
			text2 = (text2.Substring(0, 2) + text2.Substring(3, 2) + text2.Substring(6, 1)).Replace(' ', '0');
			int num = int.Parse(text2) - 2;
			if (num < 0)
			{
				num = 0;
			}
			using (FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZDoubles.dat", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				int num2 = (int)(fileStream.Length / 200);
				int num3 = num2 - 1;
				int num4 = 0;
				int num5;
				do
				{
					num5 = (num3 + num4) / 2;
					fileStream.Seek((long)num5 * 200L, SeekOrigin.Begin);
					int num6 = int.Parse(new string(binaryReader.ReadChars(200), 21, 5));
					if (num == num6)
					{
						break;
					}
					if (num < num6)
					{
						num3 = num5 - 1;
					}
					else
					{
						num4 = num5 + 1;
					}
				}
				while (num4 <= num3);
				int num7 = num5;
				if (num7 < 0)
				{
					num7 = 0;
				}
				do
				{
					fileStream.Seek((long)num7 * 200L, SeekOrigin.Begin);
					string d = new string(binaryReader.ReadChars(200));
					DoubleStarLine doubleStarLine = new DoubleStarLine();
					doubleStarLine.ReadLine(d);
					if (doubleStarLine.IsMean)
					{
						if (XZ == doubleStarLine.XZa)
						{
							text = ((doubleStarLine.Component.Trim().Length != 0) ? doubleStarLine.Component.Trim().Substring(0, 1) : "A");
						}
						if (XZ == doubleStarLine.XZb)
						{
							text = ((doubleStarLine.Component.Trim().Length != 0) ? doubleStarLine.Component.Trim().Substring(doubleStarLine.Component.Trim().Length - 1, 1) : "B");
						}
					}
					num7++;
				}
				while (num7 < num2);
			}
			LastXZforMean = XZ;
			LastComponentForMean = text;
			return text;
		}
	}
}
