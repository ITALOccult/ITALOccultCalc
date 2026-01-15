using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace Occult.Star_Catalogues
{
	public class Interferometric_Plus_WDS
	{
		private static IFindex IF_Index;

		internal static List<IFindex> Interfer_Index = new List<IFindex>();

		public static WDS_Interferometer_Display Display;

		public static AAVSO_LightCurve AAVSO;

		internal static ListMissMatches Mismatch;

		internal static ArrayList Orbits = new ArrayList();

		internal static OrbitDisplay DisplayOrbit;

		internal static int AAVSOLength = 204;

		private const double Radian = 180.0 / Math.PI;

		public static void ShowInterferometerDisplay()
		{
			try
			{
				((Control)Display).Show();
			}
			catch
			{
				Display = new WDS_Interferometer_Display();
				((Control)Display).Show();
			}
			((Control)Display).Focus();
		}

		public static void ShowLightCurve(string AAVSO_ID)
		{
			try
			{
				((Control)AAVSO).Show();
			}
			catch
			{
				AAVSO = new AAVSO_LightCurve();
				((Control)AAVSO).Show();
			}
			((Control)AAVSO.txtVar).set_Text(AAVSO_ID.Trim());
			((Control)AAVSO).Focus();
		}

		internal static void ShowDisplayOrbit()
		{
			try
			{
				((Control)DisplayOrbit).Show();
			}
			catch
			{
				DisplayOrbit = new OrbitDisplay();
				((Control)DisplayOrbit).Show();
			}
		}

		internal static void CloseInterferometerDisplay()
		{
			try
			{
				((Form)Display).Close();
				((Component)(object)Display).Dispose();
			}
			catch
			{
			}
		}

		internal static void ShowIFMismatches()
		{
			try
			{
				((Control)Mismatch).Show();
			}
			catch
			{
				Mismatch = new ListMissMatches();
				((Control)Mismatch).Show();
			}
		}

		internal static void CloseIFMismatches()
		{
			try
			{
				((Form)Display).Close();
				((Component)(object)Display).Dispose();
			}
			catch
			{
			}
		}

		internal static bool CanSearchWDS()
		{
			return File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat");
		}

		internal static bool CanSearchInterferometric()
		{
			return File.Exists(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat");
		}

		internal static void PrepareToAccessInterferometric()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!CanSearchInterferometric())
			{
				MessageBox.Show("The Interferometric Catalogue is not present. \r\n\r\nDownload the catalogue using the Maintenance  Downloads   page", "No Interferometric Catalogue");
				return;
			}
			Interfer_Index.Clear();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat");
			int num = 0;
			int num2 = 0;
			do
			{
				string text = streamReader.ReadLine();
				if (text.Substring(0, 1) != " ")
				{
					if (num > 0)
					{
						IF_Index.NumOfLines = num2;
						Interfer_Index.Add(IF_Index);
					}
					IF_Index = new IFindex();
					IF_Index.Row = num;
					IF_Index.ReadLine(text);
					num2 = 0;
				}
				if (streamReader.EndOfStream)
				{
					IF_Index.NumOfLines = num2;
					Interfer_Index.Add(IF_Index);
					break;
				}
				num++;
				num2++;
			}
			while (!streamReader.EndOfStream);
		}

		internal static void endAccessToIFcat()
		{
			Interfer_Index.Clear();
		}

		internal static string Find_WDS_IF_Matches(double RA_hrs, double Dec_deg, bool HighPrecision, bool ShowResults)
		{
			string VariableID;
			return Find_WDS_IF_Matches(RA_hrs, Dec_deg, HighPrecision, ShowResults, out VariableID);
		}

		internal static string Find_WDS_IF_Matches(double RA_hrs, double Dec_deg, bool HighPrecision, bool ShowResults, out string VariableID)
		{
			int RecNum = 0;
			bool flag = false;
			bool flag2 = true;
			double num = 0.0;
			double num2 = 0.0;
			Orbits.Clear();
			string text = "";
			VariableID = "";
			flag = false;
			num = 0.004;
			num2 = 0.04;
			double num3 = 0.0;
			if (ShowResults)
			{
				ShowInterferometerDisplay();
				Display.lstIF.get_Items().Clear();
				Display.CentreRA = RA_hrs * 15.0;
				Display.CentreDec = Dec_deg;
			}
			if (HighPrecision)
			{
				num = 0.0004;
				num2 = 0.005;
			}
			num /= Math.Cos(Dec_deg / (180.0 / Math.PI));
			string text2 = ((!HighPrecision) ? "1.2 arcmins" : "18 arcsecs");
			if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			if (Kepler2.StarInKepler2(RA_hrs * 15.0, Dec_deg, out RecNum))
			{
				if (ShowResults)
				{
					Display.lstIF.get_Items().Add((object)("Star is a Kepler2 target star. EPIC_ID = " + Kepler2.K2[RecNum].EPIC_ID));
				}
				else
				{
					text = text + "Star is a Kepler2 target star. EPIC_ID = " + Kepler2.K2[RecNum].EPIC_ID + "\r\n";
				}
			}
			int num4;
			int num5;
			int num6;
			if (CanSearchWDS())
			{
				using FileStream fileStream = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\wds.dat", FileMode.Open, FileAccess.Read);
				BinaryReader binaryReader = new BinaryReader(fileStream);
				num4 = (int)(fileStream.Length / 132 - 1);
				num5 = 0;
				flag = false;
				do
				{
					num6 = (num4 + num5) / 2;
					fileStream.Seek(132 * num6, SeekOrigin.Begin);
					string text3 = new string(binaryReader.ReadChars(10));
					double num7 = double.Parse(text3.Substring(0, 2)) + double.Parse(text3.Substring(2, 3)) / 600.0;
					if (RA_hrs > num7)
					{
						num5 = num6 + 1;
					}
					else
					{
						num4 = num6 - 1;
					}
				}
				while (num5 < num4);
				num6 -= 30;
				if (num6 < 0)
				{
					num6 = 0;
				}
				for (int i = 0; i < 400 && num6 + i < fileStream.Length / 132; i++)
				{
					fileStream.Seek(132 * (num6 + i), SeekOrigin.Begin);
					string text3 = new string(binaryReader.ReadChars(130));
					num3 = double.Parse(text3.Substring(0, 2)) + double.Parse(text3.Substring(2, 3)) / 600.0;
					double num7;
					double num8;
					if (text3.Substring(112, 6).Trim().Length > 4)
					{
						num7 = double.Parse(text3.Substring(112, 2)) + double.Parse(text3.Substring(114, 2)) / 60.0 + double.Parse(text3.Substring(116, 5).Replace(" ", "0")) / 3600.0;
						num8 = double.Parse(text3.Substring(122, 2)) + double.Parse(text3.Substring(124, 2)) / 60.0 + double.Parse(text3.Substring(126, 4).Replace(" ", "0")) / 3600.0;
						if (text3.Substring(121, 1) == "-")
						{
							num8 = 0.0 - num8;
						}
					}
					else
					{
						num7 = num3;
						num8 = double.Parse(text3.Substring(6, 2)) + double.Parse(text3.Substring(8, 2)) / 60.0;
						if (text3.Substring(5, 1) == "-")
						{
							num8 = 0.0 - num8;
						}
					}
					if ((Math.Abs(RA_hrs - num7) < num) & (Math.Abs(Dec_deg - num8) < num2))
					{
						if (!flag)
						{
							if (ShowResults)
							{
								if (Display.lstIF.get_Items().get_Count() > 0)
								{
									Display.lstIF.get_Items().Add((object)"");
									Display.lstIF.get_Items().Add((object)"------------------------------------------------");
								}
								Display.lstIF.get_Items().Add((object)"WDS entries");
								Display.lstIF.get_Items().Add((object)"");
								Display.lstIF.get_Items().Add((object)"  RA  Dec Name          Y1   Y2     N  PA  PA   Sep   Sep  M1    M2");
							}
							else
							{
								if (text.Length > 0)
								{
									text += "\r\n------------------------------------------------\r\n";
								}
								text += "WDS entries\r\n\r\n  RA  Dec Name          Y1   Y2     N  PA  PA   Sep   Sep  M1    M2\r\n";
							}
						}
						flag = true;
						if (ShowResults)
						{
							Display.lstIF.get_Items().Add((object)text3);
							if (text3.Substring(108, 1) == "O")
							{
								AddOrbitImageReference(text3.Substring(0, 10));
							}
						}
						else
						{
							text = text + text3 + "\r\n";
						}
					}
					if (num3 - RA_hrs > num)
					{
						break;
					}
				}
			}
			else if (ShowResults)
			{
				Display.lstIF.get_Items().Add((object)"WDS catalog not present");
			}
			else
			{
				text += "WDS catalog not present\r\n";
			}
			if (Interfer_Index.Count < 1)
			{
				PrepareToAccessInterferometric();
			}
			num4 = Interfer_Index.Count - 1;
			num5 = 0;
			num6 = 0;
			flag2 = true;
			if (Interfer_Index.Count > 0)
			{
				do
				{
					num6 = (num4 + num5) / 2;
					if (RA_hrs > Interfer_Index[num6].RA)
					{
						num5 = num6 + 1;
					}
					else
					{
						num4 = num6 - 1;
					}
				}
				while (num5 < num4);
				num6 -= 10;
				if (num6 < 0)
				{
					num6 = 0;
				}
				for (int j = 0; j < 20 && j + num6 < Interfer_Index.Count; j++)
				{
					if (!((Math.Abs(RA_hrs - Interfer_Index[num6 + j].RA) < num) & (Math.Abs(Dec_deg - Interfer_Index[num6 + j].Dec) < num2)))
					{
						continue;
					}
					flag = true;
					FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat", FileMode.Open, FileAccess.Read);
					BinaryReader binaryReader2 = new BinaryReader(fileStream2);
					fileStream2.Seek(120 * Interfer_Index[num6 + j].Row, SeekOrigin.Begin);
					for (int k = 0; k < Interfer_Index[num6 + j].NumOfLines; k++)
					{
						if (ShowResults)
						{
							if (flag2)
							{
								if (Display.lstIF.get_Items().get_Count() > 0)
								{
									Display.lstIF.get_Items().Add((object)"");
									Display.lstIF.get_Items().Add((object)"------------------------------------------------");
								}
								Display.lstIF.get_Items().Add((object)"Interferometric catalogue entries  [catalog has ceased]");
								Display.lstIF.get_Items().Add((object)"");
							}
							if (k == 1)
							{
								Display.lstIF.get_Items().Add((object)"    Date       PA              Sep                  M1             M2");
							}
							string text4 = new string(binaryReader2.ReadChars(120)).Substring(0, 118);
							Display.lstIF.get_Items().Add((object)text4);
							if ((k == 0) & (text4.Substring(117, 1) == "O"))
							{
								AddOrbitImageReference(text4.Substring(104, 10));
							}
						}
						else
						{
							if (flag2)
							{
								if (text.Length > 0)
								{
									text += "\r\n------------------------------------------------\r\n";
								}
								text += "Interferometric catalogue entries\r\n\r\n";
							}
							if (k == 1)
							{
								text += "    Date       PA              Sep                  M1             M2\r\n";
							}
							text = text + new string(binaryReader2.ReadChars(120)).Substring(0, 118) + "\r\n";
						}
						if (flag2)
						{
							flag2 = false;
						}
					}
					if (ShowResults)
					{
						Display.lstIF.get_Items().Add((object)"");
					}
					else
					{
						text += "\r\n";
					}
				}
			}
			else if (ShowResults)
			{
				Display.lstIF.get_Items().Add((object)"Interferometric catalog not present");
				Display.lstIF.get_Items().Add((object)"");
			}
			else
			{
				text += "Interferometric catalog not present\r\n\r\n";
			}
			if (ShowResults)
			{
				((Control)Display.cmdDisplayOrbit).set_Enabled(Orbits.Count > 0);
			}
			string text5 = XZDoubles_RetrieveEntries.XZdoubleEntries(RA_hrs, Dec_deg);
			if (ShowResults && text5.Length > 0)
			{
				if (Display.lstIF.get_Items().get_Count() > 0)
				{
					Display.lstIF.get_Items().Add((object)"");
					Display.lstIF.get_Items().Add((object)"------------------------------------------------");
				}
				Display.lstIF.get_Items().Add((object)"XZ80: Lunar occultation discoveries");
				Display.lstIF.get_Items().Add((object)"");
				Display.lstIF.get_Items().Add((object)" XZpri  XZCom  XZsys  RA   Dec  Name    Comp    Y1   Y2  N  PA    PA     Sep     Sep     M1    M2");
				string[] array = text5.Replace("\r", "").Split(new char[1] { '\n' });
				for (int l = 0; l < array.Length; l++)
				{
					Display.lstIF.get_Items().Add((object)array[l]);
				}
			}
			string text6 = IsInAAVSO(RA_hrs, Dec_deg);
			if (text6.Length > 0)
			{
				if (ShowResults)
				{
					if (Display.lstIF.get_Items().get_Count() > 0)
					{
						Display.lstIF.get_Items().Add((object)"");
						Display.lstIF.get_Items().Add((object)"------------------------------------------------");
					}
					Display.lstIF.get_Items().Add((object)"AAVSO Variable star entry");
					Display.lstIF.get_Items().Add((object)"");
					Display.lstIF.get_Items().Add((object)"       Variable identifier           Type         Max    Min  Ph  Epoch         Period ");
					Display.lstIF.get_Items().Add((object)text6);
					((Control)Display.cmdAAVSO).set_Text("Get light curve : " + text6.Substring(7, 30).Trim());
				}
				else
				{
					if (text.Length > 0)
					{
						text += "\r\n------------------------------------------------\r\n";
					}
					text += "AAVSO Variable star entry\r\n\r\n";
					text += "       Variable identifier           Type         Max    Min  Ph  Epoch         Period \r\n";
					text += text6;
				}
				VariableID = text6.Substring(7, 30).Trim();
			}
			if (ShowResults)
			{
				if (Display.lstIF.get_Items().get_Count() < 1)
				{
					Display.lstIF.get_Items().Add((object)("No entries found within " + text2 + " of " + Utilities.DEGtoDMS(RA_hrs, 2, 1, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(Dec_deg, 3, 0, MinutesOnly: false)));
				}
				((Control)Display).Focus();
			}
			else if (text.Length == 0)
			{
				text = "No entries found within " + text2 + " of " + Utilities.DEGtoDMS(RA_hrs, 2, 1, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(Dec_deg, 3, 0, MinutesOnly: false);
			}
			return text;
		}

		internal static bool IsInWDS(double RA_hrs, double Dec_deg, bool HighPrecision)
		{
			int num = 1;
			int num2 = 0;
			double num3 = 0.0;
			double num4 = 0.0025;
			double num5 = 0.02;
			if (HighPrecision)
			{
				num4 = 0.0004;
				num5 = 0.005;
			}
			num4 /= Math.Cos(Dec_deg / 57.2957795);
			bool result = false;
			if (CanSearchWDS())
			{
				using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\wds.dat", FileMode.Open, FileAccess.Read))
				{
					BinaryReader binaryReader = new BinaryReader(fileStream);
					num = (int)(fileStream.Length / 132 - 1);
					num2 = 0;
					result = false;
					try
					{
						int num6;
						do
						{
							num6 = (num + num2) / 2;
							fileStream.Seek(132 * num6, SeekOrigin.Begin);
							string text = new string(binaryReader.ReadChars(10));
							double num7 = double.Parse(text.Substring(0, 2)) + double.Parse(text.Substring(2, 3)) / 600.0;
							if (RA_hrs > num7)
							{
								num2 = num6 + 1;
							}
							else
							{
								num = num6 - 1;
							}
						}
						while (num2 < num);
						num6 -= 20;
						if (num6 < 0)
						{
							num6 = 0;
						}
						int num8 = 0;
						while (true)
						{
							if (num8 >= 400)
							{
								return result;
							}
							if (num6 + num8 >= fileStream.Length / 132)
							{
								break;
							}
							fileStream.Seek(132 * (num6 + num8), SeekOrigin.Begin);
							string text = new string(binaryReader.ReadChars(130));
							num3 = double.Parse(text.Substring(0, 2)) + (double.Parse(text.Substring(2, 3)) - 2.0) / 600.0;
							double num7;
							double num9;
							if (text.Substring(112, 6).Trim().Length > 4)
							{
								num7 = double.Parse(text.Substring(112, 2)) + double.Parse(text.Substring(114, 2)) / 60.0 + double.Parse(text.Substring(116, 5).Replace(" ", "0")) / 3600.0;
								num9 = double.Parse(text.Substring(122, 2)) + double.Parse(text.Substring(124, 2)) / 60.0 + double.Parse(text.Substring(126, 4).Replace(" ", "0")) / 3600.0;
								if (text.Substring(121, 1) == "-")
								{
									num9 = 0.0 - num9;
								}
							}
							else
							{
								num7 = num3;
								num9 = double.Parse(text.Substring(6, 2)) + double.Parse(text.Substring(8, 2)) / 60.0;
								if (text.Substring(5, 1) == "-")
								{
									num9 = 0.0 - num9;
								}
							}
							if ((Math.Abs(RA_hrs - num7) < num4) & (Math.Abs(Dec_deg - num9) < num5))
							{
								return true;
							}
							if (num3 - RA_hrs > num4)
							{
								return result;
							}
							num8++;
						}
						return result;
					}
					catch
					{
						return false;
					}
				}
			}
			return result;
		}

		internal static bool IsInInterferometric(double RA_hrs, double Dec_deg, bool HighPrecision)
		{
			bool result = false;
			if (!CanSearchInterferometric())
			{
				return result;
			}
			if (Interfer_Index.Count < 1)
			{
				PrepareToAccessInterferometric();
			}
			int num = Interfer_Index.Count - 1;
			int num2 = 0;
			double num3 = 0.0025;
			double num4 = 0.02;
			if (HighPrecision)
			{
				num3 = 0.0004;
				num4 = 0.005;
			}
			num3 /= Math.Cos(Dec_deg / 57.2957795);
			if (Interfer_Index.Count > 0)
			{
				int num5;
				do
				{
					num5 = (num + num2) / 2;
					if (RA_hrs > Interfer_Index[num5].RA)
					{
						num2 = num5 + 1;
					}
					else
					{
						num = num5 - 1;
					}
				}
				while (num2 < num);
				num5 -= 10;
				if (num5 < 0)
				{
					num5 = 0;
				}
				for (int i = 0; i < 20 && num5 + i < Interfer_Index.Count; i++)
				{
					if ((Math.Abs(RA_hrs - Interfer_Index[num5 + i].RA) < num3) & (Math.Abs(Dec_deg - Interfer_Index[num5 + i].Dec) < num4))
					{
						result = true;
						break;
					}
				}
			}
			return result;
		}

		internal static int StarIsInDoubleCats(double RA_hrs, double Dec_deg)
		{
			int num = 0;
			if (IsInWDS(RA_hrs, Dec_deg, HighPrecision: true))
			{
				num++;
			}
			if (IsInInterferometric(RA_hrs, Dec_deg, HighPrecision: true))
			{
				num += 2;
			}
			return num;
		}

		internal static bool IsPositivelyDoubleInInterferometric(double RA_hrs, double Dec_deg, bool HighPrecision)
		{
			if (Interfer_Index.Count < 1)
			{
				PrepareToAccessInterferometric();
			}
			int num = Interfer_Index.Count - 1;
			int num2 = 0;
			bool flag = false;
			double num3 = 0.0025;
			double num4 = 0.02;
			if (HighPrecision)
			{
				num3 = 0.0004;
				num4 = 0.005;
			}
			num3 /= Math.Cos(Dec_deg / 57.2957795);
			if (Interfer_Index.Count > 0)
			{
				int num5;
				do
				{
					num5 = (num + num2) / 2;
					if (RA_hrs > Interfer_Index[num5].RA)
					{
						num2 = num5 + 1;
					}
					else
					{
						num = num5 - 1;
					}
				}
				while (num2 < num);
				num5 -= 10;
				if (num5 < 0)
				{
					num5 = 0;
				}
				for (int i = 0; i < 200 && num5 + i < Interfer_Index.Count && !(Interfer_Index[num5 + i].RA - RA_hrs > num3); i++)
				{
					if (!((Math.Abs(RA_hrs - Interfer_Index[num5 + i].RA) < num3) & (Math.Abs(Dec_deg - Interfer_Index[num5 + i].Dec) < num4)))
					{
						continue;
					}
					FileStream fileStream = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat", FileMode.Open, FileAccess.Read);
					BinaryReader binaryReader = new BinaryReader(fileStream);
					fileStream.Seek(120 * Interfer_Index[num5 + i].Row, SeekOrigin.Begin);
					new string(binaryReader.ReadChars(120)).Substring(0, 118);
					for (int j = 1; j < Interfer_Index[num5 + i].NumOfLines; j++)
					{
						string text = new string(binaryReader.ReadChars(120)).Substring(0, 118);
						if (!(text.Substring(111, 3) == "Occ") && !"<>?SU".Contains(text.Substring(28, 1)))
						{
							if (!double.TryParse(text.Substring(29, 9).Replace(" ", "0"), out var result))
							{
								result = 0.0;
							}
							if (result < 1.0 && result > 0.0)
							{
								flag = true;
								break;
							}
						}
					}
					if (flag)
					{
						break;
					}
				}
			}
			return flag;
		}

		internal static string IsInAAVSO(double RA_hrs, double Dec_deg)
		{
			string result = "";
			if (!File.Exists(Utilities.AppPath + "\\Downloaded Files\\AAVSOindex.dat"))
			{
				return result;
			}
			if (AAVSOLength < 150)
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Downloaded Files\\AAVSOindex.dat");
				AAVSOLength = streamReader.ReadLine()!.Length + 2;
			}
			if (AAVSOLength != 204)
			{
				return result;
			}
			using FileStream fileStream = new FileStream(Utilities.AppPath + "\\Downloaded Files\\AAVSOindex.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num = (int)fileStream.Length / AAVSOLength - 1;
			int num2 = num;
			int num3 = 0;
			double num4 = RA_hrs * 15.0;
			int num5;
			do
			{
				num5 = (num2 + num3) / 2;
				fileStream.Seek(num5 * AAVSOLength, SeekOrigin.Begin);
				string text = new string(binaryReader.ReadChars(AAVSOLength - 2));
				double num6 = double.Parse(text.Substring(41, 9));
				if (num4 > num6)
				{
					num3 = num5 + 1;
				}
				else
				{
					num2 = num5 - 1;
				}
			}
			while (num3 < num2);
			num5 -= 3;
			if (num5 < 0)
			{
				num5 = 0;
			}
			int num7 = 0;
			while (true)
			{
				if (num7 < 20)
				{
					if (num5 + num7 >= num)
					{
						break;
					}
					fileStream.Seek((num5 + num7) * AAVSOLength, SeekOrigin.Begin);
					string text = new string(binaryReader.ReadChars(AAVSOLength - 2));
					if (!(text.Substring(39, 1) == "2"))
					{
						double num6 = double.Parse(text.Substring(41, 9));
						double num8 = double.Parse(text.Substring(51, 9));
						if ((Math.Abs(num4 - num6) < 0.001) & (Math.Abs(Dec_deg - num8) < 0.001))
						{
							StringBuilder stringBuilder = new StringBuilder();
							stringBuilder.Append("".PadRight(7) + text.Substring(8, 30));
							stringBuilder.Append(text.Substring(61, 11));
							stringBuilder.Append(text.Substring(94, 6));
							stringBuilder.Append(" ");
							stringBuilder.Append(text.Substring(117, 6) + " ");
							stringBuilder.Append(text.Substring(127, 2));
							stringBuilder.Append(text.Substring(137, 12) + " ");
							stringBuilder.Append(text.Substring(155, 14));
							stringBuilder.Append("".PadRight(2));
							return stringBuilder.ToString();
						}
					}
					num7++;
					continue;
				}
				return result;
			}
			return result;
		}

		internal static void CreateXZVariables()
		{
			ArrayList arrayList = new ArrayList();
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.Read))
			{
				BinaryReader readXZ = new BinaryReader(fileStream);
				int num = (int)fileStream.Length / 35;
				PBar pBar = new PBar();
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(num);
				((Control)pBar).set_Text("Creating new file of XZVariables");
				((Control)pBar).Show();
				for (int i = 0; i < num; i++)
				{
					if (i % 1000 == 0)
					{
						pBar.pBarFTP.set_Value(i);
						Application.DoEvents();
					}
					XZ80Q.ReadStarEntry(fileStream, readXZ, i);
					string text = IsInAAVSO(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, XZ80Q.Dec_rad * (180.0 / Math.PI));
					if (text.Length > 1)
					{
						arrayList.Add(XZ80Q.XZ.ToString().PadLeft(6) + text.Substring(6));
					}
				}
				((Component)(object)pBar).Dispose();
			}
			arrayList.Sort();
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource files\\XZVariables.dat", append: false);
			for (int j = 0; j < arrayList.Count; j++)
			{
				streamWriter.WriteLine(arrayList[j]!.ToString());
			}
		}

		internal static void AddOrbitImageReference(string Coords)
		{
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt"))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt");
			for (int i = 0; i < 7; i++)
			{
				string text = streamReader.ReadLine();
			}
			do
			{
				string text = streamReader.ReadLine();
				if (text.Substring(249, 10) == Coords)
				{
					Orbits.Add(text.Substring(246, 18));
				}
			}
			while (!streamReader.EndOfStream);
		}
	}
}
