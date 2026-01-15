using System;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;
using Occult.File_Actions;

namespace Occult
{
	public class JPL_DE
	{
		private static int[] StartPointer = new int[13]
		{
			3, 171, 231, 309, 342, 366, 387, 405, 423, 441,
			753, 819, 899
		};

		private static int[] NumOfCoeff = new int[13]
		{
			14, 10, 13, 11, 8, 7, 6, 6, 6, 13,
			11, 10, 10
		};

		private static int[] NumOfIntervals = new int[13]
		{
			4, 2, 2, 1, 1, 1, 1, 1, 1, 8,
			2, 4, 4
		};

		private static int[] Interval = new int[13]
		{
			8, 16, 16, 32, 32, 32, 32, 32, 32, 4,
			16, 8, 8
		};

		public static string AppPath;

		public static string DE_Ephemeris_Version = "DE_Ephemeris.bin";

		internal static int EphemerisVersion = 0;

		internal static int LongEphemerisVersion = 0;

		public static string DE_LongEphemeris_Version = "DE_LongEphemeris.bin";

		internal static bool DE_EphemerisAvailable = false;

		internal static bool DE_LongEphemerisAvailable = false;

		public static double JDStart = 2305424.5;

		public static double JDEnd = 2525008.5;

		public static double JDStartLong = 2305424.5;

		public static double JDEndLong = 2525008.5;

		private static bool DEFile_Open = false;

		private const double AUkm = 149597870.7;

		private const double EarthMoonRatio = 81.30056907419062;

		private const double EarthFromBarycentreRatio = 0.012150584269940352;

		private const double UnitLightTime = 0.00577551833;

		internal static bool Initialised = false;

		public static Create_JPL_DE_EphemerisFile CreateDEfile;

		private static FileStream DE_File;

		private static BinaryReader DE_Read;

		private static string Ephemeris_ToUse = "";

		private static int Version;

		public static void Show_CreateJPL_DE(int Tab)
		{
			Create_JPL_DE_EphemerisFile.OpeningTab = Tab;
			try
			{
				((Control)CreateDEfile).Show();
			}
			catch
			{
				CreateDEfile = new Create_JPL_DE_EphemerisFile();
				((Control)CreateDEfile).Show();
			}
		}

		public static bool DE_Ephemeris(double JD, int Planet, bool Barycentric, out double x2000, out double y2000, out double z2000, out int Version)
		{
			double num = 0.0;
			if (!Initialised)
			{
				InitialiseDE_Ephemeris();
			}
			double[] array = new double[14];
			double[] array2 = new double[14];
			double[] array3 = new double[14];
			double[] array4 = new double[14];
			x2000 = (y2000 = (z2000 = (Version = 0)));
			if (Planet > 11 || Planet < 1)
			{
				return false;
			}
			string text;
			long num2;
			if ((JD >= JDStart) & (JD < JDEnd))
			{
				text = DE_Ephemeris_Version;
				Version = EphemerisVersion;
				num2 = (long)(int)(Math.Floor((JD - JDStart) / 32.0) + 2.0) * 1018L * 8;
				num = (JD - JDStart) % 32.0;
			}
			else
			{
				if (!((JD >= JDStartLong) & (JD < JDEndLong)))
				{
					return false;
				}
				text = DE_LongEphemeris_Version;
				Version = LongEphemerisVersion;
				num2 = (long)(int)(Math.Floor((JD - JDStartLong) / 32.0) + 2.0) * 1018L * 8;
				num = (JD - JDStartLong) % 32.0;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + text, FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num3 = (int)num / Interval[Planet - 1];
			int num4 = NumOfCoeff[Planet - 1];
			fileStream.Seek(num2 + 8 * (StartPointer[Planet - 1] - 1) + 24 * NumOfCoeff[Planet - 1] * num3, SeekOrigin.Begin);
			for (int i = 0; i < num4; i++)
			{
				array[i] = binaryReader.ReadDouble();
			}
			for (int j = 0; j < num4; j++)
			{
				array2[j] = binaryReader.ReadDouble();
			}
			for (int k = 0; k < num4; k++)
			{
				array3[k] = binaryReader.ReadDouble();
			}
			double num5 = (num - (double)(num3 * Interval[Planet - 1])) / (double)Interval[Planet - 1];
			double num6 = 2.0 * num5 - 1.0;
			double num7 = 2.0 * num6;
			array4[0] = 1.0;
			array4[1] = num6;
			for (int l = 2; l < num4; l++)
			{
				array4[l] = num7 * array4[l - 1] - array4[l - 2];
			}
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			for (int m = 0; m < num4; m++)
			{
				num8 += array[m] * array4[m];
			}
			for (int n = 0; n < num4; n++)
			{
				num9 += array2[n] * array4[n];
			}
			for (int num11 = 0; num11 < num4; num11++)
			{
				num10 += array3[num11] * array4[num11];
			}
			num3 = (int)num / Interval[2];
			num4 = NumOfCoeff[2];
			fileStream.Seek(num2 + 8 * (StartPointer[2] - 1) + 24 * NumOfCoeff[2] * num3, SeekOrigin.Begin);
			for (int num12 = 0; num12 < num4; num12++)
			{
				array[num12] = binaryReader.ReadDouble();
			}
			for (int num13 = 0; num13 < num4; num13++)
			{
				array2[num13] = binaryReader.ReadDouble();
			}
			for (int num14 = 0; num14 < num4; num14++)
			{
				array3[num14] = binaryReader.ReadDouble();
			}
			num5 = (num - (double)(num3 * Interval[2])) / (double)Interval[2];
			num6 = 2.0 * num5 - 1.0;
			num7 = 2.0 * num6;
			array4[0] = 1.0;
			array4[1] = num6;
			for (int num15 = 2; num15 < num4; num15++)
			{
				array4[num15] = num7 * array4[num15 - 1] - array4[num15 - 2];
			}
			double num16 = 0.0;
			double num17 = 0.0;
			double num18 = 0.0;
			for (int num19 = 0; num19 < num4; num19++)
			{
				num16 += array[num19] * array4[num19];
			}
			for (int num20 = 0; num20 < num4; num20++)
			{
				num17 += array2[num20] * array4[num20];
			}
			for (int num21 = 0; num21 < num4; num21++)
			{
				num18 += array3[num21] * array4[num21];
			}
			num3 = (int)num / Interval[9];
			num4 = NumOfCoeff[9];
			fileStream.Seek(num2 + 8 * (StartPointer[9] - 1) + 24 * NumOfCoeff[9] * num3, SeekOrigin.Begin);
			for (int num22 = 0; num22 < num4; num22++)
			{
				array[num22] = binaryReader.ReadDouble();
			}
			for (int num23 = 0; num23 < num4; num23++)
			{
				array2[num23] = binaryReader.ReadDouble();
			}
			for (int num24 = 0; num24 < num4; num24++)
			{
				array3[num24] = binaryReader.ReadDouble();
			}
			if (Interval[9] != Interval[Planet - 1])
			{
				num5 = (num - (double)(num3 * Interval[9])) / (double)Interval[9];
				num6 = 2.0 * num5 - 1.0;
				num7 = 2.0 * num6;
				array4[0] = 1.0;
				array4[1] = num6;
				for (int num25 = 2; num25 < num4; num25++)
				{
					array4[num25] = num7 * array4[num25 - 1] - array4[num25 - 2];
				}
			}
			double num26 = 0.0;
			double num27 = 0.0;
			double num28 = 0.0;
			for (int num29 = 0; num29 < num4; num29++)
			{
				num26 -= 0.012150584269940352 * array[num29] * array4[num29];
			}
			for (int num30 = 0; num30 < num4; num30++)
			{
				num27 -= 0.012150584269940352 * array2[num30] * array4[num30];
			}
			for (int num31 = 0; num31 < num4; num31++)
			{
				num28 -= 0.012150584269940352 * array3[num31] * array4[num31];
			}
			num16 += num26;
			num17 += num27;
			num18 += num28;
			double num32 = Math.Sqrt(num16 * num16 + num17 * num17 + num18 * num18) / 149597870.7 * 0.00577551833;
			if (Planet < 10 && !Barycentric)
			{
				num3 = (int)num / Interval[10];
				num4 = NumOfCoeff[10];
				fileStream.Seek(num2 + 8 * (StartPointer[10] - 1) + 24 * NumOfCoeff[10] * num3, SeekOrigin.Begin);
				for (int num33 = 0; num33 < num4; num33++)
				{
					array[num33] = binaryReader.ReadDouble();
				}
				for (int num34 = 0; num34 < num4; num34++)
				{
					array2[num34] = binaryReader.ReadDouble();
				}
				for (int num35 = 0; num35 < num4; num35++)
				{
					array3[num35] = binaryReader.ReadDouble();
				}
				num5 = (num - (double)(num3 * Interval[10]) - num32) / (double)Interval[10];
				num6 = 2.0 * num5 - 1.0;
				num7 = 2.0 * num6;
				array4[0] = 1.0;
				array4[1] = num6;
				for (int num36 = 2; num36 < num4; num36++)
				{
					array4[num36] = num7 * array4[num36 - 1] - array4[num36 - 2];
				}
				for (int num37 = 0; num37 < num4; num37++)
				{
					num8 -= array[num37] * array4[num37];
				}
				for (int num38 = 0; num38 < num4; num38++)
				{
					num9 -= array2[num38] * array4[num38];
				}
				for (int num39 = 0; num39 < num4; num39++)
				{
					num10 -= array3[num39] * array4[num39];
				}
			}
			if (Planet == 3)
			{
				num8 += num26;
				num9 += num27;
				num10 += num28;
			}
			if (Planet == 10)
			{
				for (int num40 = 0; num40 < 3; num40++)
				{
					double num41 = Math.Sqrt(num8 * num8 + num9 * num9 + num10 * num10) / 149597870.7 * 0.00577551833;
					num5 = (num - (double)(num3 * Interval[Planet - 1]) - num41) / (double)Interval[Planet - 1];
					num6 = 2.0 * num5 - 1.0;
					num7 = 2.0 * num6;
					array4[0] = 1.0;
					array4[1] = num6;
					for (int num42 = 2; num42 < num4; num42++)
					{
						array4[num42] = num7 * array4[num42 - 1] - array4[num42 - 2];
					}
					num8 = (num9 = (num10 = 0.0));
					for (int num43 = 0; num43 < num4; num43++)
					{
						num8 += array[num43] * array4[num43];
					}
					for (int num44 = 0; num44 < num4; num44++)
					{
						num9 += array2[num44] * array4[num44];
					}
					for (int num45 = 0; num45 < num4; num45++)
					{
						num10 += array3[num45] * array4[num45];
					}
					x2000 = num8;
					y2000 = num9;
					z2000 = num10;
				}
			}
			else
			{
				x2000 = num8 / 149597870.7;
				y2000 = num9 / 149597870.7;
				z2000 = num10 / 149597870.7;
			}
			fileStream.Close();
			return true;
		}

		internal static void InitialiseDE_Ephemeris()
		{
			char[] array = new char[200];
			int num = 0;
			if (File.Exists(AppPath + "\\Resource Files\\" + DE_Ephemeris_Version))
			{
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + DE_Ephemeris_Version);
				streamReader.ReadBlock(array, 0, 200);
				string text = new string(array, 0, 200).ToUpper();
				num = text.IndexOf("DE") + 2;
				if (!int.TryParse(text.Substring(num, 3), out EphemerisVersion))
				{
					EphemerisVersion = 0;
				}
				num = text.IndexOf("=");
				JDStart = double.Parse(text.Substring(num + 1, 11));
				num = text.IndexOf("=", num + 1);
				JDEnd = double.Parse(text.Substring(num + 1, 11));
				DE_EphemerisAvailable = true;
			}
			else
			{
				DE_EphemerisAvailable = false;
				JDStart = 0.0;
				JDEnd = -1.0;
			}
			if (File.Exists(AppPath + "\\Resource Files\\" + DE_LongEphemeris_Version))
			{
				using StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\" + DE_LongEphemeris_Version);
				streamReader2.ReadBlock(array, 0, 200);
				string text2 = new string(array, 0, 200).ToUpper();
				num = text2.IndexOf("DE") + 2;
				if (!int.TryParse(text2.Substring(num, 3), out LongEphemerisVersion))
				{
					LongEphemerisVersion = 0;
				}
				num = text2.IndexOf("=");
				JDStartLong = double.Parse(text2.Substring(num + 1, 11));
				num = text2.IndexOf("=", num + 1);
				JDEndLong = double.Parse(text2.Substring(num + 1, 11));
				DE_LongEphemerisAvailable = true;
			}
			else
			{
				DE_LongEphemerisAvailable = false;
				JDStartLong = 0.0;
				JDEndLong = -1.0;
			}
			Initialised = true;
		}

		internal static bool OpenDEFile(double JD)
		{
			if (DEFile_Open)
			{
				return true;
			}
			if (!Initialised)
			{
				InitialiseDE_Ephemeris();
			}
			if ((JD >= JDStart) & (JD < JDEnd))
			{
				Ephemeris_ToUse = DE_Ephemeris_Version;
				Version = EphemerisVersion;
			}
			else
			{
				if (!((JD >= JDStartLong) & (JD < JDEndLong)))
				{
					return false;
				}
				Ephemeris_ToUse = DE_LongEphemeris_Version;
				Version = LongEphemerisVersion;
			}
			DE_File = new FileStream(AppPath + "\\Resource Files\\" + Ephemeris_ToUse, FileMode.Open, FileAccess.Read);
			DE_Read = new BinaryReader(DE_File);
			DEFile_Open = true;
			return true;
		}

		internal static void CloseDEFile()
		{
			if (DEFile_Open)
			{
				DE_File.Close();
			}
			DEFile_Open = false;
		}

		public static void DE_AberrationCoefficients(double JD, out double dX, out double dY, out double dZ)
		{
			double num = 0.01;
			dX = (dY = (dZ = 0.0));
			if (OpenDEFile(JD))
			{
				Earth_FromSolarBarycenter(JD - num, out var X_b, out var Y_b, out var Z_b);
				Earth_FromSolarBarycenter(JD + num, out var X_b2, out var Y_b2, out var Z_b2);
				dX = 0.5 / num * (X_b2 - X_b);
				dY = 0.5 / num * (Y_b2 - Y_b);
				dZ = 0.5 / num * (Z_b2 - Z_b);
				CloseDEFile();
			}
		}

		internal static bool Lunar_Position(double JD_TT, out double x, out double y, out double z, out int EphemVersion)
		{
			x = (y = (z = 0.0));
			double num = 1E-05;
			EphemVersion = Version;
			if (!OpenDEFile(JD_TT))
			{
				return false;
			}
			for (int i = 0; i < 3; i++)
			{
				GetCoords(JD_TT - num, 9, out x, out y, out z);
				num = Math.Sqrt(x * x + y * y + z * z) / 299792.458 / 86400.0;
			}
			CloseDEFile();
			return true;
		}

		private static void Earth_FromEarthMoonBarycenter(double JD_TT, out double X_EMb, out double Y_EMb, out double Z_EMb)
		{
			GetCoords(JD_TT, 9, out var x, out var y, out var z);
			X_EMb = (0.0 - x) * 0.012150584269940352 / 149597870.7;
			Y_EMb = (0.0 - y) * 0.012150584269940352 / 149597870.7;
			Z_EMb = (0.0 - z) * 0.012150584269940352 / 149597870.7;
		}

		internal static void Sun_FromBaryCenter(double JD_TT, out double Sun_BaryX, out double Sun_BaryY, out double Sun_BaryZ)
		{
			GetCoords(JD_TT, 10, out Sun_BaryX, out Sun_BaryY, out Sun_BaryZ);
			Sun_BaryX /= 149597870.7;
			Sun_BaryY /= 149597870.7;
			Sun_BaryZ /= 149597870.7;
		}

		internal static bool Planet_PositionFromEarth(double JD_TT, int Planet, out double x, out double y, out double z, out double PlanetRadiusVector, out double AstrometricDistance, out double TrueDistance, out int EphemVersion)
		{
			x = (y = (z = 0.0));
			double Xplanet = 0.0;
			double Yplanet = 0.0;
			double Zplanet = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			AstrometricDistance = (TrueDistance = 1.0);
			double num4 = 0.0;
			EphemVersion = Version;
			PlanetRadiusVector = 1.0;
			if (!OpenDEFile(JD_TT))
			{
				return false;
			}
			if (Planet == 3)
			{
				Sun_FromEarth_ForHeliocentricOrbits(JD_TT, 0.0, out x, out y, out z, out var _);
			}
			else
			{
				Earth_FromEarthMoonBarycenter(JD_TT, out var X_EMb, out var Y_EMb, out var Z_EMb);
				EarthMoonBarycenter_FromSolarBarycenter(JD_TT, out var XEMb, out var YEMb, out var ZEMb);
				for (int i = 0; i < 3; i++)
				{
					Planet_FromSolarBaryCenter(JD_TT - num4, Planet, out Xplanet, out Yplanet, out Zplanet);
					num = Xplanet - XEMb - X_EMb;
					num2 = Yplanet - YEMb - Y_EMb;
					num3 = Zplanet - ZEMb - Z_EMb;
					num4 = Math.Sqrt(num * num + num2 * num2 + num3 * num3) * 0.00577551833;
				}
				Sun_FromBaryCenter(JD_TT - num4, out var Sun_BaryX, out var Sun_BaryY, out var Sun_BaryZ);
				Xplanet -= Sun_BaryX;
				Yplanet -= Sun_BaryY;
				Zplanet -= Sun_BaryZ;
				PlanetRadiusVector = Math.Sqrt(Xplanet * Xplanet + Yplanet * Yplanet + Zplanet * Zplanet);
				x = num;
				y = num2;
				z = num3;
				AstrometricDistance = Math.Sqrt(x * x + y * y + z * z);
				Earth_FromEarthMoonBarycenter(JD_TT - num4, out var X_EMb2, out var Y_EMb2, out var Z_EMb2);
				EarthMoonBarycenter_FromSolarBarycenter(JD_TT - num4, out var XEMb2, out var YEMb2, out var ZEMb2);
				double num5 = x + XEMb + X_EMb - XEMb2 - X_EMb2;
				double num6 = y + YEMb + Y_EMb - YEMb2 - Y_EMb2;
				double num7 = z + ZEMb + Z_EMb - ZEMb2 - Z_EMb2;
				TrueDistance = Math.Sqrt(num5 * num5 + num6 * num6 + num7 * num7);
			}
			CloseDEFile();
			return true;
		}

		internal static bool Sun_FromEarth_ForHeliocentricOrbits(double JD_TT, double ObjectDistanceAU, out double x, out double y, out double z, out int EphemVersion)
		{
			x = (y = (z = 0.0));
			Version = 1;
			bool result = Sun_FromEarth_ForHeliocentricOrbits(JD_TT, FromSun: true, ForJWST: false, ObjectDistanceAU, out x, out y, out z, out Version);
			EphemVersion = Version;
			return result;
		}

		internal static bool Barycenter_FromEarth(double JD_TT, double ObjectDistanceAU, out double x, out double y, out double z, out int EphemVersion)
		{
			x = (y = (z = 0.0));
			Version = 1;
			bool result = Sun_FromEarth_ForHeliocentricOrbits(JD_TT, FromSun: false, ForJWST: false, ObjectDistanceAU, out x, out y, out z, out Version);
			EphemVersion = Version;
			return result;
		}

		internal static bool Sun_FromEarth_ForHeliocentricOrbits(double JD_TT, bool FromSun, bool ForJWST, double ObjectDistanceAU, out double x, out double y, out double z, out int EphemVersion)
		{
			x = (y = (z = 0.0));
			EphemVersion = Version;
			if (!OpenDEFile(JD_TT))
			{
				return false;
			}
			Earth_FromEarthMoonBarycenter(JD_TT, out var X_EMb, out var Y_EMb, out var Z_EMb);
			EarthMoonBarycenter_FromSolarBarycenter(JD_TT, out var XEMb, out var YEMb, out var ZEMb);
			Sun_FromBaryCenter(JD_TT - ObjectDistanceAU * 0.00577551833, out var Sun_BaryX, out var Sun_BaryY, out var Sun_BaryZ);
			CloseDEFile();
			if (FromSun)
			{
				x = XEMb + X_EMb - Sun_BaryX;
				y = YEMb + Y_EMb - Sun_BaryY;
				z = ZEMb + Z_EMb - Sun_BaryZ;
			}
			else
			{
				x = XEMb + X_EMb;
				y = YEMb + Y_EMb;
				z = ZEMb + Z_EMb;
			}
			if (ForJWST)
			{
				JamesWeb.GetJWST_OffsetFromEarth(JD_TT, x, y, z, out var jX, out var jY, out var jZ);
				x += jX;
				y += jY;
				z += jZ;
			}
			return true;
		}

		internal static bool Planet_XYZCoords(double JD_TT, int Planet, bool Heliocentric, out double x, out double y, out double z, out int EphemVersion)
		{
			EphemVersion = Version;
			x = (y = (z = 0.0));
			if (!OpenDEFile(JD_TT))
			{
				return false;
			}
			if (Planet == 3)
			{
				Sun_FromEarth_ForHeliocentricOrbits(JD_TT, 0.0, out x, out y, out z, out var _);
			}
			else
			{
				Planet_FromSolarBaryCenter(JD_TT, Planet, out x, out y, out z);
				if (Heliocentric)
				{
					Sun_FromBaryCenter(JD_TT, out var Sun_BaryX, out var Sun_BaryY, out var Sun_BaryZ);
					x -= Sun_BaryX;
					y -= Sun_BaryY;
					z -= Sun_BaryZ;
				}
			}
			CloseDEFile();
			return true;
		}

		public static void DE_HelioXYZ_AllPLanets(string JDstartNNNN)
		{
			double x = 0.0;
			double y = 0.0;
			double z = 0.0;
			string text = JDstartNNNN;
			text = text.Trim() + "0000";
			double num = Convert.ToDouble(text) + 0.5;
			OpenDEFile(num);
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + text + "_XYZ.bin", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Generating daily planetary XYZ from JD" + text);
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(10001);
			((Control)pBar).Show();
			for (int i = 0; i < 10000; i++)
			{
				if (i % 100 == 0)
				{
					pBar.pBarFTP.set_Value(i);
				}
				double jD_TT = num + (double)i;
				GetCoords(jD_TT, 10, out var x2, out var y2, out var z2);
				x2 /= 149597870.7;
				y2 /= 149597870.7;
				z2 /= 149597870.7;
				Math.Sqrt(x2 * x2 + y2 * y2 + z2 * z2);
				for (int j = 1; j < 10; j++)
				{
					GetCoords(jD_TT, j - 1, out x, out y, out z);
					x /= 149597870.7;
					y /= 149597870.7;
					z /= 149597870.7;
					Math.Sqrt(x * x + y * y + z * z);
					if (j == 3)
					{
						binaryWriter.Write(x - x2);
						binaryWriter.Write(y - y2);
						binaryWriter.Write(z - z2);
					}
					else
					{
						binaryWriter.Write(Convert.ToSingle(x - x2));
						binaryWriter.Write(Convert.ToSingle(y - y2));
						binaryWriter.Write(Convert.ToSingle(z - z2));
					}
				}
			}
			fileStream.Close();
			CloseDEFile();
			((Form)pBar).Close();
			((Component)(object)pBar).Dispose();
		}

		private static void EarthMoonBarycenter_FromSolarBarycenter(double JD_TT, out double XEMb, out double YEMb, out double ZEMb)
		{
			GetCoords(JD_TT, 2, out XEMb, out YEMb, out ZEMb);
			XEMb /= 149597870.7;
			YEMb /= 149597870.7;
			ZEMb /= 149597870.7;
		}

		private static void Earth_FromSolarBarycenter(double JD_TT, out double X_b, out double Y_b, out double Z_b)
		{
			GetCoords(JD_TT, 2, out var x, out var y, out var z);
			Earth_FromEarthMoonBarycenter(JD_TT, out var X_EMb, out var Y_EMb, out var Z_EMb);
			X_b = x / 149597870.7 + X_EMb;
			Y_b = y / 149597870.7 + Y_EMb;
			Z_b = z / 149597870.7 + Z_EMb;
		}

		private static void Planet_FromSolarBaryCenter(double JD_TT, int Planet, out double Xplanet, out double Yplanet, out double Zplanet)
		{
			if (Planet == 3)
			{
				Earth_FromSolarBarycenter(JD_TT, out Xplanet, out Yplanet, out Zplanet);
			}
			else
			{
				int @object = 0;
				if (Planet == 0)
				{
					@object = 10;
				}
				else if (Planet < 10)
				{
					@object = Planet - 1;
				}
				GetCoords(JD_TT, @object, out Xplanet, out Yplanet, out Zplanet);
			}
			Xplanet /= 149597870.7;
			Yplanet /= 149597870.7;
			Zplanet /= 149597870.7;
		}

		private static void GetCoords(double JD_TT, int Object, out double x, out double y, out double z)
		{
			long num = 0L;
			double num2 = 0.0;
			double[] array = new double[14];
			double[] array2 = new double[14];
			double[] array3 = new double[14];
			double[] array4 = new double[14];
			if ((JD_TT >= JDStart) & (JD_TT < JDEnd))
			{
				Ephemeris_ToUse = DE_Ephemeris_Version;
				num = (long)(int)(Math.Floor((JD_TT - JDStart) / 32.0) + 2.0) * 1018L * 8;
				num2 = (JD_TT - JDStart) % 32.0;
			}
			else if ((JD_TT >= JDStartLong) & (JD_TT < JDEndLong))
			{
				Ephemeris_ToUse = DE_LongEphemeris_Version;
				num = (long)(int)(Math.Floor((JD_TT - JDStartLong) / 32.0) + 2.0) * 1018L * 8;
				num2 = (JD_TT - JDStartLong) % 32.0;
			}
			int num3 = (int)num2 / Interval[Object];
			int num4 = NumOfCoeff[Object];
			DE_File.Seek(num + 8 * (StartPointer[Object] - 1) + 24 * NumOfCoeff[Object] * num3, SeekOrigin.Begin);
			for (int i = 0; i < num4; i++)
			{
				array[i] = DE_Read.ReadDouble();
			}
			for (int j = 0; j < num4; j++)
			{
				array2[j] = DE_Read.ReadDouble();
			}
			for (int k = 0; k < num4; k++)
			{
				array3[k] = DE_Read.ReadDouble();
			}
			double num5 = (num2 - (double)(num3 * Interval[Object])) / (double)Interval[Object];
			double num6 = 2.0 * num5 - 1.0;
			double num7 = 2.0 * num6;
			array4[0] = 1.0;
			array4[1] = num6;
			x = (y = (z = 0.0));
			for (int l = 2; l < num4; l++)
			{
				array4[l] = num7 * array4[l - 1] - array4[l - 2];
			}
			for (int m = 0; m < num4; m++)
			{
				x += array[m] * array4[m];
			}
			for (int n = 0; n < num4; n++)
			{
				y += array2[n] * array4[n];
			}
			for (int num8 = 0; num8 < num4; num8++)
			{
				z += array3[num8] * array4[num8];
			}
		}

		public static bool DE_Nutation(double JD, out double NutL, out double NutE, out int Version)
		{
			double num = 0.0;
			if (!Initialised)
			{
				InitialiseDE_Ephemeris();
			}
			double[] array = new double[14];
			double[] array2 = new double[14];
			double[] array3 = new double[14];
			NutL = (NutE = (Version = 0));
			string text;
			long num2;
			if ((JD >= JDStart) & (JD < JDEnd))
			{
				text = DE_Ephemeris_Version;
				Version = EphemerisVersion;
				num2 = (long)(int)(Math.Floor((JD - JDStart) / 32.0) + 2.0) * 1018L * 8;
				num = (JD - JDStart) % 32.0;
			}
			else
			{
				if (!((JD >= JDStartLong) & (JD < JDEndLong)))
				{
					return false;
				}
				text = DE_LongEphemeris_Version;
				Version = LongEphemerisVersion;
				num2 = (long)(int)(Math.Floor((JD - JDStartLong) / 32.0) + 2.0) * 1018L * 8;
				num = (JD - JDStartLong) % 32.0;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + text, FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num3 = (int)num / Interval[11];
			int num4 = NumOfCoeff[11];
			fileStream.Seek(num2 + 8 * (StartPointer[11] - 1) + 16 * NumOfCoeff[11] * num3, SeekOrigin.Begin);
			for (int i = 0; i < num4; i++)
			{
				array[i] = binaryReader.ReadDouble();
			}
			if ((array[0] == 0.0) & (array[1] == 0.0))
			{
				return false;
			}
			for (int j = 0; j < num4; j++)
			{
				array2[j] = binaryReader.ReadDouble();
			}
			double num5 = (num - (double)(num3 * Interval[11])) / (double)Interval[11];
			double num6 = 2.0 * num5 - 1.0;
			double num7 = 2.0 * num6;
			array3[0] = 1.0;
			array3[1] = num6;
			for (int k = 2; k < num4; k++)
			{
				array3[k] = num7 * array3[k - 1] - array3[k - 2];
			}
			for (int l = 0; l < num4; l++)
			{
				NutL += array[l] * array3[l];
			}
			for (int m = 0; m < num4; m++)
			{
				NutE += array2[m] * array3[m];
			}
			fileStream.Close();
			return true;
		}

		internal static string GetDE_VersionDetails(string DE_Version)
		{
			string text = "(?/?)";
			string text2 = "?";
			if (DE_Version.Length > 1)
			{
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + DE_Version);
				char[] array = new char[256];
				streamReader.Read(array, 0, 256);
				string text3 = new string(array);
				int num = text3.IndexOf("DE");
				if (num >= 0)
				{
					text2 = text3.Substring(num, 5);
					num = text3.IndexOf("DE", num + 2);
					if (num >= 0 && num < 80)
					{
						text2 = text2 + "," + text3.Substring(num + 2, 3);
					}
				}
				else
				{
					text2 = "???";
				}
				text = "(" + (ExtractYear(text3.Substring(84, 80)) + 1) + "/" + ExtractYear(text3.Substring(168, 80)) + ")";
			}
			return text2 + " " + text;
		}

		private static int ExtractYear(string Ln)
		{
			int num = Ln.IndexOf(".") + 2;
			int num2 = Ln.IndexOf(" ", num + 1);
			if (num2 - num > 8)
			{
				num2 = Ln.IndexOf("-", num + 1);
			}
			if (!int.TryParse(Ln.Substring(num, num2 - num), out var result))
			{
				return 0;
			}
			return result;
		}

		internal static bool Set_DE_EphemerisFile(string DE_File)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!ReNameEphemerisToDEVersion(IsLongEphemFile: false))
			{
				MessageBox.Show("Can't rename the current DE_Ephemeris file\r\n\r\nTherefore cannot set a new file for the DE_LongEphemeris", "Can't rename", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			string sourceFileName = AppPath + "\\Resource Files\\" + DE_File;
			string destFileName = AppPath + "\\Resource Files\\DE_Ephemeris.bin";
			File.Move(sourceFileName, destFileName);
			InitialiseDE_Ephemeris();
			return true;
		}

		internal static bool Set_DE_LongEphemerisFile(string DE_File)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!ReNameEphemerisToDEVersion(IsLongEphemFile: true))
			{
				MessageBox.Show("Can't rename the current DE_LongEphemeris file\r\n\r\nTherefore cannot set a new file for the DE_LongEphemeris", "Can't rename", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			string sourceFileName = AppPath + "\\Resource Files\\" + DE_File;
			string destFileName = AppPath + "\\Resource Files\\DE_LongEphemeris.bin";
			File.Move(sourceFileName, destFileName);
			InitialiseDE_Ephemeris();
			return true;
		}

		internal static bool ReNameEphemerisToDEVersion(bool IsLongEphemFile)
		{
			string[] array = new string[8] { "", "a", "b", "c", "d", "e", "f", "g" };
			string text = AppPath + "\\Resource Files\\";
			string text2 = text + "DE_Ephemeris.bin";
			string text3 = "";
			bool flag = false;
			if (IsLongEphemFile)
			{
				text2 = text + "DE_LongEphemeris.bin";
			}
			if (!File.Exists(text2))
			{
				return true;
			}
			char[] array2 = new char[80];
			int num = -1;
			int num2 = -1;
			string text4 = "";
			using (StreamReader streamReader = new StreamReader(text2))
			{
				streamReader.ReadBlock(array2, 0, 80);
				string text5 = new string(array2, 20, 50).ToUpper();
				num = text5.IndexOf("DE");
				if (num >= 0)
				{
					text4 = text5.Substring(num, 5);
				}
				num2 = text5.IndexOf("DE", num + 1);
				if (num2 >= 0)
				{
					text4 = text4 + "_" + text5.Substring(num2 + 2, 3);
				}
			}
			for (int i = 0; i < array.Length; i++)
			{
				text3 = text4 + array[i] + ".bin";
				flag = !File.Exists(text + text3);
				if (flag)
				{
					break;
				}
			}
			if (!flag | (text3.Length < 1))
			{
				return false;
			}
			File.Move(text2, text + text3);
			return true;
		}

		internal static void PurgeLunarCache()
		{
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\Moon", "Moon*.bin");
			for (int i = 0; i < files.Length; i++)
			{
				File.Delete(files[i]);
			}
		}
	}
}
