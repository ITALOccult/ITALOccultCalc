using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.File_Actions;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	internal class MinorPlanetOccultationElements
	{
		internal static int MaximumNumEvents = (int)Settings.Default.MaximumSearchEvents;

		public static string AppPath;

		public static List<AsteroidElements> ElementsAt1Day = new List<AsteroidElements>();

		public static AsteroidSearch CallingForm;

		public static bool AbortFlag = false;

		internal static List<BinaryAsteroidElements> BinaryElements = new List<BinaryAsteroidElements>();

		internal static List<int[]> BinaryAsteroidsInMiriade = new List<int[]>();

		private static int BinaryFirstUnnumbered = 0;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double ParallaxAtUnitDistance = 4.26352124542639E-05;

		private const double UnitLightTime = 0.0057756;

		private const double GaussK = 0.01720209895;

		private const double k0 = 172020.9895;

		private const int ElementFileLength = 145;

		private const int UserStarCatFileLength = 40;

		private const double MaxSearchDistance_EarthRadii = 50.0;

		internal static bool SearchWDS = false;

		internal static bool SearchIF = false;

		internal static bool IncludeWDSSearchInSearch = true;

		internal static bool QueryMiriade = true;

		internal static bool MiriadeFirstOnly = true;

		internal static bool SaveMiriadeResponse = false;

		public static bool? MiriadeWasQueried = null;

		public static int NumberOfPredictionsForBinarySystem = 1;

		internal static double OffsetRA = 0.0;

		internal static double OffsetDec = 0.0;

		internal static double MagDropLimitForAsteroidSearches = -1.0;

		internal static bool ApplySiteRestriction = false;

		internal static double SiteLongitudeTest = 0.0;

		internal static double SiteLatitudeTest = 0.0;

		internal static double SearchDistance = 0.2;

		internal static bool UseHorizonsEphemeris = false;

		internal static bool UseOldHorizonsOrbit = false;

		internal static string OldHorizonsOrbitForQueryString = "";

		public static bool LimitSearchToKepler2 = false;

		internal const double StandardExposureDurn = 0.0367;

		internal static double SingleExposureMultiplierToGetToMag;

		internal static double NumberOfExposuresRequired_ToBeSure;

		internal static List<Nearby> Near = new List<Nearby>();

		internal Nearby NearStar;

		internal static string NearbyStarList = "";

		internal static string GaiaFileForNearby = "";

		internal static void CreateBinaryElementList()
		{
			BinaryAsteroids.Fill_AllAsteroids();
		}

		internal static void CreateBinaryAstroidsInMiriade()
		{
			string[] array = Settings.Default.MiriadeAsteroids.Split(new char[1] { ' ' });
			List<int> list = new List<int>();
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length <= 0)
				{
					continue;
				}
				int num = int.Parse(array[i]);
				BinaryAsteroids.GetAsteroidRecord_fromNumberName(num, "", list);
				int num2 = 0;
				for (int j = 0; j < list.Count; j++)
				{
					if (BinaryAsteroids.BinElements[list[j]].A > (double)num2)
					{
						num2 = Convert.ToInt32(BinaryAsteroids.BinElements[list[j]].A);
					}
				}
				int[] item = new int[2] { num, num2 };
				BinaryAsteroidsInMiriade.Add(item);
			}
		}

		internal static string GetBinaryAsteroidData(double AsteriodNumber, string AsteroidID, int NumOfMoons)
		{
			string text = "";
			int num = 0;
			if (NumOfMoons % 10 > 0)
			{
				if (AsteriodNumber > 0.0)
				{
					for (int i = 0; i < BinaryFirstUnnumbered; i++)
					{
						if (AsteriodNumber == BinaryElements[i].IDAsteroidNumber)
						{
							text += string.Format("{0,4:F0}{1,6:F0}    ", BinaryElements[i].Diameter, BinaryElements[i].A);
							num++;
							if (num >= NumOfMoons)
							{
								break;
							}
						}
					}
				}
				else
				{
					for (int j = BinaryFirstUnnumbered; j < BinaryElements.Count; j++)
					{
						if (AsteroidID.Trim() == BinaryElements[j].IDAsteroidIdentifier.Trim())
						{
							text += string.Format("{0,4:F0}{1,6:F0}    ", BinaryElements[j].Diameter, BinaryElements[j].A);
							num++;
							if (num >= NumOfMoons)
							{
								break;
							}
						}
					}
				}
			}
			return text.PadRight(28);
		}

		internal static void BessellianElements(double RAStar, double DecStar, double RAAsteroid, double DecAsteroid, double DistanceAsteroid, out double X, out double Y)
		{
			double num = 4.26352124542639E-05 / DistanceAsteroid;
			X = Math.Cos(DecAsteroid) * Math.Sin(RAAsteroid - RAStar) / num;
			Y = (Math.Sin(DecAsteroid) * Math.Cos(DecStar) - Math.Cos(DecAsteroid) * Math.Sin(DecStar) * Math.Cos(RAAsteroid - RAStar)) / num;
		}

		internal static void Integrate(List<AsteroidElements> ElementsList, int RecordNum, double StartJD, double EndJD, bool AddNotIntegratedFlag)
		{
			bool flag = true;
			double num = 0.15;
			double num2 = 5.0;
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			int num3 = 0;
			int num4 = 0;
			double num5 = 0.0;
			int iDNumber = ElementsList[RecordNum].IDNumber;
			string iDName = ElementsList[RecordNum].IDName;
			double OsculatingDate = ElementsList[RecordNum].OsculatingJD;
			int epochYear = ElementsList[RecordNum].EpochYear;
			int epochMonth = ElementsList[RecordNum].EpochMonth;
			double epochDay = ElementsList[RecordNum].EpochDay;
			double EpochMeanAnomaly = ElementsList[RecordNum].Meananomaly / (180.0 / Math.PI);
			double perihelion = ElementsList[RecordNum].Perihelion / (180.0 / Math.PI);
			double node = ElementsList[RecordNum].Node / (180.0 / Math.PI);
			double i = ElementsList[RecordNum].I / (180.0 / Math.PI);
			double e = ElementsList[RecordNum].E;
			double a = ElementsList[RecordNum].A;
			num5 = Math.Abs(ElementsList[RecordNum].q);
			double h = ElementsList[RecordNum].H0;
			num = ElementsList[RecordNum].G_phaseCoeff;
			num2 = ElementsList[RecordNum].LogR_Coeff;
			double diameter_Mean = ElementsList[RecordNum].Diameter_Mean;
			string dia_Source = ElementsList[RecordNum].Dia_Source;
			double peakEphemUncert = ElementsList[RecordNum].PeakEphemUncert;
			num3 = ElementsList[RecordNum].Num_Rings;
			num4 = ElementsList[RecordNum].Num_Moons;
			string orbitSource = ElementsList[RecordNum].OrbitSource;
			string orbitDate = ElementsList[RecordNum].OrbitDate;
			string asteroidClass = ElementsList[RecordNum].AsteroidClass;
			double errorMajor = ElementsList[RecordNum].ErrorMajor;
			double errorMinor = ElementsList[RecordNum].ErrorMinor;
			double errorPA = ElementsList[RecordNum].ErrorPA;
			Utilities.PositionfromElements(OsculatingDate, 0.0, 0.0, 0.0, 0.0, OsculatingDate, EpochMeanAnomaly, num5, e, perihelion, node, i, h, num, num2, 0.0, out var _, out var _, out var _, out var AstrometricGeocentricDistance, out var _, out var _, out var _);
			flag = (OsculatingDate - 0.5) % 1.0 == 0.0;
			if (e >= 0.97 || AstrometricGeocentricDistance < 0.3)
			{
				flag = false;
			}
			if (flag)
			{
				if (OsculatingDate != StartJD - 1.0)
				{
					Utilities.NumericIntegrate(StartJD - 1.0, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref num5, ref e, ref i, ref node, ref perihelion, saveflag: false, iDNumber, iDName, h, num, num2, diameter_Mean, dia_Source, peakEphemUncert, num3, num4, orbitSource, orbitDate, asteroidClass, errorMajor, errorMinor, errorPA, ElementsAt1Day);
				}
				Utilities.NumericIntegrate(EndJD, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref num5, ref e, ref i, ref node, ref perihelion, saveflag: true, iDNumber, iDName, h, num, num2, diameter_Mean, dia_Source, peakEphemUncert, num3, num4, orbitSource, orbitDate, asteroidClass, errorMajor, errorMinor, errorPA, ElementsAt1Day);
				return;
			}
			ElementsAt1Day.Clear();
			for (double num6 = StartJD; num6 <= EndJD; num6 += 1.0)
			{
				AsteroidElements asteroidElements = new AsteroidElements();
				double num8;
				if (e < 0.97)
				{
					double num7 = 0.01720209895 * Math.Pow(a, -1.5);
					for (num8 = (EpochMeanAnomaly + num7 * (num6 - OsculatingDate)) * (180.0 / Math.PI); num8 < 0.0; num8 += 360.0)
					{
					}
					while (num8 >= 360.0)
					{
						num8 -= 360.0;
					}
					Utilities.Date_from_JD(num6, out Year, out Month, out day);
				}
				else
				{
					num8 = 0.0;
					Year = epochYear;
					Month = epochMonth;
					day = epochDay;
				}
				asteroidElements.IDNumber = iDNumber;
				asteroidElements.IDName = iDName;
				asteroidElements.Meananomaly = num8;
				asteroidElements.EpochYear = Year;
				asteroidElements.EpochMonth = Month;
				asteroidElements.EpochDay = day;
				asteroidElements.Perihelion = perihelion * (180.0 / Math.PI);
				asteroidElements.Node = node * (180.0 / Math.PI);
				asteroidElements.I = i * (180.0 / Math.PI);
				asteroidElements.E = e;
				asteroidElements.A = a;
				asteroidElements.q = a * (1.0 - e);
				if (e > 0.97)
				{
					asteroidElements.q = a;
				}
				asteroidElements.H0 = h;
				asteroidElements.G_phaseCoeff = num;
				asteroidElements.LogR_Coeff = num2;
				asteroidElements.Diameter_Mean = diameter_Mean;
				asteroidElements.Dia_Source = dia_Source;
				asteroidElements.PeakEphemUncert = peakEphemUncert;
				asteroidElements.Num_Rings = num3;
				asteroidElements.Num_Moons = num4;
				asteroidElements.OrbitSource = orbitSource;
				asteroidElements.OrbitDate = orbitDate;
				asteroidElements.AsteroidClass = asteroidClass;
				asteroidElements.ErrorMajor = errorMajor;
				asteroidElements.ErrorMinor = errorMinor;
				asteroidElements.ErrorPA = errorPA;
				ElementsAt1Day.Add(asteroidElements);
			}
		}

		internal static void MinorPlanetSearch_Gaia(string GaiaCatBaseName, double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDuration, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			MinorPlanetSearch_Gaia(GaiaCatBaseName, StartJD, EndJD, FirstAsteroidRecord, LastAsteroidRecord, MinimumAsteroidDiameter, AsteroidClass, LimitingStarMagForSearch, MinimumDuration, 0.0, VarExposure: false, ExpandedSearchDistInEarthRadii, UseUserElementFile, UseDefaultErrorSettings, AppendFlag, UserElementsFile, AutoSaveSearch, OutputFileName, ShowProgressBar, GetNearbyStars: false, ShowListAndDisplay: true);
		}

		internal static void MinorPlanetSearch_Gaia(string GaiaCatBaseName, double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDurationSet, double TelescopeAperture_cm, bool VarExposure, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar, bool GetNearbyStars, bool ShowListAndDisplay)
		{
			int num = 0;
			bool flag = false;
			double num2 = 1.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 1.0;
			double Magnitude = 20.0;
			bool flag2 = false;
			bool flag3 = false;
			_ = new char[141];
			double num10 = 0.0;
			long num11 = 0L;
			long num12 = 0L;
			long num13 = 0L;
			int[] array = new int[6];
			_ = new string[5];
			int num14 = 27;
			bool flag4 = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			List<AsteroidElements> astElements;
			if (!UseUserElementFile)
			{
				astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else
			{
				astElements = Elements.UserAsteroids.AstElements;
				Elements.UserAsteroids.SourceFile = UserElementsFile;
				Elements.UserAsteroids.Fill_AllAsteroids();
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Gaia\\" + GaiaCatBaseName + ".inx", FileMode.Open, FileAccess.Read);
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\Gaia\\" + GaiaCatBaseName + ".bin", FileMode.Open, FileAccess.Read);
			if (GaiaCatBaseName.Contains("DR3"))
			{
				Gaia.CurrentRecordLength = 58L;
			}
			else
			{
				Gaia.CurrentRecordLength = 48L;
			}
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			long num15 = (int)(fileStream2.Length / Gaia.RecordLength);
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum(LastAsteroidRecord - FirstAsteroidRecord + 1);
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			for (int i = FirstAsteroidRecord; i <= LastAsteroidRecord; i++)
			{
				flag = false;
				if (DisplayMPOccultations.OccElements.Count > MaximumNumEvents)
				{
					AbortFlag = true;
				}
				if ((AsteroidClass.Length > 0 && !astElements[i].AsteroidClass.Contains(AsteroidClass)) || (astElements[i].EpochYear < 2015 && ShowProgressBar && !CallingForm.chkHorizons.get_Checked()))
				{
					continue;
				}
				num2 = astElements[i].Diameter_Mean;
				int NumMeasures;
				if (ShowProgressBar)
				{
					CallingForm.pbarSearch.set_Value(i - FirstAsteroidRecord);
					CallingForm.lstResults.get_Items().Clear();
					Label lblAsteroidName = CallingForm.lblAsteroidName;
					NumMeasures = astElements[i].IDNumber;
					((Control)lblAsteroidName).set_Text(NumMeasures + " " + astElements[i].IDName.Trim());
					Application.DoEvents();
				}
				if (!(num2 >= MinimumAsteroidDiameter || FirstAsteroidRecord == LastAsteroidRecord))
				{
					continue;
				}
				try
				{
					Integrate(astElements, i, StartJD, EndJD, AddNotIntegratedFlag: true);
				}
				catch
				{
					continue;
				}
				for (int j = 0; j < ElementsAt1Day.Count - 1; j++)
				{
					_ = ElementsAt1Day[j].IDNumber;
					_ = ElementsAt1Day[j].IDName;
					int epochYear = ElementsAt1Day[j].EpochYear;
					int epochMonth = ElementsAt1Day[j].EpochMonth;
					double epochDay = ElementsAt1Day[j].EpochDay;
					double meanAnomaly = ElementsAt1Day[j].Meananomaly / (180.0 / Math.PI);
					double perihelion = ElementsAt1Day[j].Perihelion / (180.0 / Math.PI);
					double node = ElementsAt1Day[j].Node / (180.0 / Math.PI);
					double i2 = ElementsAt1Day[j].I / (180.0 / Math.PI);
					double e = ElementsAt1Day[j].E;
					double q = ElementsAt1Day[j].A * (1.0 - e);
					double h = ElementsAt1Day[j].H0;
					double g_phaseCoeff = ElementsAt1Day[j].G_phaseCoeff;
					double logR_Coeff = ElementsAt1Day[j].LogR_Coeff;
					num2 = ElementsAt1Day[j].Diameter_Mean;
					_ = ElementsAt1Day[j].Dia_Source;
					double num16 = ElementsAt1Day[j].PeakEphemUncert;
					num3 = ElementsAt1Day[j].ErrorMajor;
					num4 = ElementsAt1Day[j].ErrorMinor;
					num5 = ElementsAt1Day[j].ErrorPA;
					if (j == 0)
					{
						num13 = 0L;
						num12 = 0L;
						num11 = 0L;
					}
					if (e > 0.97)
					{
						q = Math.Abs(ElementsAt1Day[j].q);
						meanAnomaly = 0.0;
					}
					double num17 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
					double num18 = ((!(e < 0.97)) ? (StartJD + (double)j) : num17);
					Utilities.PositionfromElements(num18, 0.0, 0.0, 0.0, 0.0, num17, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out Magnitude, out var Elongation, out var PhaseAngle);
					Utilities.PositionfromElements(num18 + 0.01, 0.0, 0.0, 0.0, 0.0, num17, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA2, out var _, out var RadiusVector2, out var AstrometricGeocentricDistance2, out var Magnitude2, out var Elongation2, out var PhaseAngle2);
					if (Math.Abs(RA2 - RA) < Math.PI)
					{
						flag2 = RA2 > RA;
					}
					Utilities.PositionfromElements(num18 + 1.0, 0.0, 0.0, 0.0, 0.0, num17, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA3, out var Dec3, out RadiusVector, out var AstrometricGeocentricDistance3, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(num18 + 0.99, 0.0, 0.0, 0.0, 0.0, num17, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA4, out var _, out PhaseAngle2, out Elongation2, out Magnitude2, out AstrometricGeocentricDistance2, out RadiusVector2);
					if (Math.Abs(RA3 - RA4) < Math.PI)
					{
						flag3 = RA3 > RA4;
					}
					bool num19 = flag2 != flag3;
					double num20 = (num18 - 2451545.0) / 365.25;
					double num21 = 0.002 + 0.0028 / AstrometricGeocentricDistance;
					double A = RA;
					double B = RA3;
					if (!num19)
					{
						if (flag2 && B < A)
						{
							B += Math.PI * 2.0;
						}
						if (!flag2 && B > A)
						{
							B -= Math.PI * 2.0;
						}
					}
					if (Math.Abs(B - A) < Math.PI)
					{
						if (A > B)
						{
							Utilities.Swap(ref A, ref B);
						}
					}
					else if (A < B)
					{
						Utilities.Swap(ref A, ref B);
					}
					double A2 = Dec;
					double B2 = Dec3;
					if (A2 > B2)
					{
						Utilities.Swap(ref A2, ref B2);
					}
					double num22 = 0.1 + 0.003 / AstrometricGeocentricDistance;
					double num23 = num16 / 8.794143836182533 * AstrometricGeocentricDistance3;
					if (num23 > 0.7)
					{
						num23 = 0.7;
					}
					int num24 = 179 - (int)(360.0 / Math.PI * B2 + 2.0 * num22);
					if (B2 < 0.0)
					{
						num24++;
					}
					if (num24 < 0)
					{
						num24 = 0;
					}
					int num25 = 179 - (int)(360.0 / Math.PI * A2 - 2.0 * num22);
					if (A2 < 0.0)
					{
						num25++;
					}
					if (num25 > 359)
					{
						num25 = 359;
					}
					int num26 = 0;
					for (int k = num24; k <= num25; k++)
					{
						double num27 = (double)(180 - k) / 2.0;
						double num28 = num27 - 0.5;
						int num29 = 361 * k;
						int num30 = (int)Math.Floor(A * (180.0 / Math.PI) - 0.02);
						if (num30 < 0)
						{
							num30 += 360;
						}
						if (num30 > 360)
						{
							num30 -= 360;
						}
						int num31 = (int)Math.Ceiling(B * (180.0 / Math.PI) + 0.02);
						if (num31 > 360)
						{
							num31 -= 360;
						}
						if (num31 < 0)
						{
							num31 += 360;
						}
						fileStream.Seek(num29 * 4, SeekOrigin.Begin);
						array[0] = binaryReader.ReadInt32() - num14;
						if (array[0] < 0)
						{
							array[0] = 0;
						}
						fileStream.Seek((num29 + num30) * 4, SeekOrigin.Begin);
						array[1] = binaryReader.ReadInt32() - 1;
						if (array[1] < 0)
						{
							array[1] = 0;
						}
						fileStream.Seek((num29 + num31) * 4, SeekOrigin.Begin);
						array[2] = binaryReader.ReadInt32();
						fileStream.Seek((num29 + 360) * 4, SeekOrigin.Begin);
						array[3] = binaryReader.ReadInt32();
						int num32 = array[1];
						int num33 = array[2];
						double num34 = 0.0;
						if (num32 >= num33)
						{
							num32 = array[0];
							num33 = array[2];
							num34 = Math.PI * 2.0;
						}
						double num35 = B;
						double num36 = A - num34;
						do
						{
							IL_09c8:
							if (AbortFlag)
							{
								binaryReader.Close();
								binaryReader2.Close();
								if (ShowProgressBar)
								{
									((Control)CallingForm.pbarSearch).set_Visible(false);
									((Control)CallingForm.cmdCompute).set_Visible(true);
									CallingForm.SearchCancelled = true;
								}
								return;
							}
							if (num32 < num15)
							{
								if (!Gaia.ReadNext(fileStream2, binaryReader2, num32))
								{
									num32++;
								}
								else if (Gaia.gaiaVersionOfStar == 1 && ((Gaia.Dec_deg > num27) | (Gaia.Dec_deg < num28)))
								{
									num32++;
								}
								else
								{
									double RA5;
									double num37 = (RA5 = Gaia.RA_rad);
									double num38 = (num10 = Gaia.Dec_rad);
									if (RA5 > num36 - num21 && RA5 < num35 + num21 && num10 > A2 - num21 && num10 < B2 + num21)
									{
										double magGreen = Gaia.MagGreen;
										double num39 = -2.5 * Math.Log10(Math.Pow(10.0, magGreen / -2.5) + Math.Pow(10.0, Magnitude / -2.5));
										double num40 = Magnitude - num39;
										if ((magGreen <= LimitingStarMagForSearch) & (num40 >= MagDropLimitForAsteroidSearches))
										{
											long num41 = Gaia.StarNumber;
											if (num41 == 0 || (num41 != num11 && num41 != num12 && num41 != num13))
											{
												double pMRA_rad = Gaia.PMRA_rad;
												double pMDec_rad = Gaia.PMDec_rad;
												_ = Gaia.Reliability;
												_ = 1.4;
												Utilities.ProperMotion(ref RA5, ref num10, pMRA_rad, pMDec_rad, Gaia.Parallax_rad, Gaia.RadialVelocityKmSec, num20 - Gaia.Epoch_2000);
												Gaia.Gaia_FrameRotationCorrections(Gaia.gaiaVersionOfStar, num18, RA5, num10, magGreen, out var dRA_asec, out var dDec_asec, out RadiusVector2, out AstrometricGeocentricDistance2);
												RA5 += dRA_asec * Math.Cos(num10) / 3600.0 / (180.0 / Math.PI);
												num10 += dDec_asec / 3600.0 / (180.0 / Math.PI);
												flag4 = true;
												if (LimitSearchToKepler2)
												{
													if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
													{
														Kepler2.Initialise_Kepler2_ForAsteroids();
													}
													flag4 = Kepler2.StarInKepler2(RA5 * (180.0 / Math.PI), num10 * (180.0 / Math.PI));
												}
												if (flag4)
												{
													BessellianElements(RA5, num10, RA, Dec, AstrometricGeocentricDistance, out var X, out var Y);
													BessellianElements(RA5, num10, RA3, Dec3, AstrometricGeocentricDistance3, out var X2, out var Y2);
													double num42 = X2 - X;
													double num43 = Y2 - Y;
													double num44 = Math.Sqrt(num42 * num42 + num43 * num43);
													double num45 = X * num42 + Y * num43;
													double value = (X * num43 - Y * num42) / num44;
													double num46 = Math.Floor((0.0 - num45) / num44 / num44 * 24.0);
													double num47 = num2 / num44 * 86400.0 / 6378.137;
													bool flag5 = true;
													SingleExposureMultiplierToGetToMag = 1.0;
													NumberOfExposuresRequired_ToBeSure = 3.0;
													if (TelescopeAperture_cm > 0.0)
													{
														double num48 = 5.5 + 5.0 * Math.Log10(TelescopeAperture_cm);
														if (VarExposure)
														{
															if (magGreen > num48)
															{
																SingleExposureMultiplierToGetToMag = Math.Pow(2.512, magGreen - num48);
															}
															else
															{
																SingleExposureMultiplierToGetToMag = 1.0;
															}
															if (num40 <= 0.4)
															{
																NumberOfExposuresRequired_ToBeSure = 0.75 / num40 / num40;
															}
															flag5 = num47 > 0.0367 * SingleExposureMultiplierToGetToMag * NumberOfExposuresRequired_ToBeSure;
														}
														else
														{
															flag5 = (magGreen < num48) & (num47 < 0.0367 * NumberOfExposuresRequired_ToBeSure);
														}
													}
													if (((num47 > MinimumDurationSet && flag5) & (Math.Abs(value) < 10.0 + num23 + ExpandedSearchDistInEarthRadii)) && num46 > -8.0 && num46 < 32.0)
													{
														string starID = Gaia.StarID;
														double parallax_rad = Gaia.Parallax_rad;
														double magRed = Gaia.MagRed;
														double magBlue = Gaia.MagBlue;
														double num49 = Gaia.SigmaRA_arcsecs(num20);
														double num50 = Gaia.SigmaDec_arcsecs(num20);
														double num51 = 0.0;
														string errorBasis;
														if (num3 > 0.0)
														{
															double num52 = num5 / (180.0 / Math.PI);
															double num53 = num3 * Math.Sin(num52);
															double num54 = num3 * Math.Cos(num52);
															double num55 = num4 * Math.Sin(num52 - Math.PI / 2.0);
															double num56 = num4 * Math.Cos(num52 - Math.PI / 2.0);
															double num57 = Math.Sqrt(num53 * num53 + num49 * num49);
															if (num53 < 0.0)
															{
																num57 = 0.0 - num57;
															}
															double num58 = Math.Sqrt(num54 * num54 + num50 * num50);
															if (num54 < 0.0)
															{
																num58 = 0.0 - num58;
															}
															double num59 = Math.Sqrt(num55 * num55 + num49 * num49);
															if (num55 < 0.0)
															{
																num59 = 0.0 - num59;
															}
															double num60 = Math.Sqrt(num56 * num56 + num50 * num50);
															if (num56 < 0.0)
															{
																num60 = 0.0 - num60;
															}
															num6 = Math.Sqrt(num57 * num57 + num58 * num58);
															num7 = Math.Sqrt(num59 * num59 + num60 * num60);
															num8 = Math.Atan2(num57, num58) * (180.0 / Math.PI);
															errorBasis = "Known errors";
															double num61 = Math.Atan2(num42, num43) * (180.0 / Math.PI);
															if (num61 > 180.0)
															{
																num61 = (num61 -= 180.0);
															}
															if (num61 < 0.0)
															{
																num61 += 180.0;
															}
															double num62 = 0.0;
															double num63 = num7 / num6;
															double angle_deg = num8 - num61;
															for (int l = -90; l <= 90; l += 2)
															{
																double x = num6 * Math.Sin((double)l / (180.0 / Math.PI));
																double y = num63 * num6 * Math.Cos((double)l / (180.0 / Math.PI));
																Utilities.RotateXY(x, y, angle_deg, out var _, out var y2);
																if (y2 > num62)
																{
																	num62 = y2;
																}
															}
															double num64 = num62 / 8.794143836182533 * AstrometricGeocentricDistance * 6378.137;
															num9 = (num2 + num64) / num2;
															num51 = num16;
														}
														else if (num16 == 0.0)
														{
															num16 = 0.2;
															num9 = 0.0;
															num51 = 0.0;
															errorBasis = "Star+Assumed";
														}
														else
														{
															errorBasis = "Star+PeakEphemUncert";
															errorBasis += Gaia.SourceOfData;
															num6 = Math.Sqrt(num16 * num16 + num49 * num49);
															num7 = Math.Sqrt(num16 * num16 + num50 * num50);
															num8 = 90.0;
															num9 = 0.0;
															num51 = 0.0;
														}
														double starDiaMAS = Gaia.StarDiameter_mas;
														string Basis;
														bool InvalidDiameter;
														double num65 = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS((num37 - Gaia.PMRA_rad * Gaia.Epoch_2000) * (180.0 / Math.PI) / 15.0, (num38 - Gaia.PMDec_rad * Gaia.Epoch_2000) * (180.0 / Math.PI), magGreen, magBlue, magRed, out Basis, out NumMeasures, out InvalidDiameter);
														if (num65 > 1.0)
														{
															starDiaMAS = num65;
														}
														double starReliability = Gaia.Reliability;
														if (Gaia.GaiaVersionOfStar == 0)
														{
															starReliability = -4.0;
														}
														if (Gaia.HipPositionFrome_UBSC)
														{
															starReliability = -3.0;
														}
														if (Gaia.HipPositionUnreliable_UBSC)
														{
															starReliability = -2.0;
														}
														int noGaiaProperMotion = Gaia.NoGaiaProperMotion;
														int properMotionUsingUCAC = Gaia.ProperMotionUsingUCAC4;
														int duplicateSource = Gaia.DuplicateSource;
														MinorPlanetPrediction(num18, num46, j, starID, RA5, num10, parallax_rad, magGreen, magBlue, magRed, num6, num7, num8, num49, num50, num51, num9, errorBasis, num23 + ExpandedSearchDistInEarthRadii, starDiaMAS, starReliability, noGaiaProperMotion, properMotionUsingUCAC, duplicateSource, GetNearbyStars, out flag, UseDefaultErrorSettings, AutoSaveSearch, OutputFileName, ShowListAndDisplay);
														num++;
														if (flag)
														{
															num13 = num12;
															num12 = num11;
															num11 = num41;
															num26++;
														}
													}
												}
											}
										}
									}
									num32++;
								}
								if (num32 <= num33)
								{
									goto IL_09c8;
								}
							}
							if (num34 == 0.0)
							{
								break;
							}
							num34 = 0.0;
							num32 = array[1];
							num33 = array[3];
							num35 += Math.PI * 2.0;
							num36 += Math.PI * 2.0;
						}
						while (num34 >= 0.0);
					}
					if (num26 == 0)
					{
						num13 = num12;
						num12 = num11;
						num11 = 0L;
					}
				}
			}
			binaryReader.Close();
			binaryReader2.Close();
			if (ShowProgressBar)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		internal static void MinorPlanetSearch_Tycho2(double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDuration, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, bool UseTycho2, string UserElementsFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar, string StarCatalogueBase)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double Magnitude = 20.0;
			bool flag2 = false;
			bool flag3 = false;
			_ = new char[141];
			double num8 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			int num9 = 27;
			bool flag4 = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			List<AsteroidElements> astElements;
			if (!UseUserElementFile)
			{
				astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else
			{
				astElements = Elements.UserAsteroids.AstElements;
				Elements.UserAsteroids.SourceFile = UserElementsFile;
				if ((astElements.Count < 1) | CallingForm.chkHorizons.get_Checked())
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
			}
			Tycho2.RecordLength = 27;
			FileStream fileStream;
			FileStream fileStream2;
			if (UseTycho2)
			{
				fileStream = new FileStream(AppPath + "\\Resource Files\\Tycho2.inx", FileMode.Open, FileAccess.Read);
				fileStream2 = new FileStream(AppPath + "\\Resource Files\\Tycho2.bin", FileMode.Open, FileAccess.Read);
				if (fileStream2.Length > 99000000)
				{
					Tycho2.RecordLength = 41;
				}
			}
			else
			{
				fileStream = new FileStream(AppPath + "\\Resource Files\\" + StarCatalogueBase + ".inx", FileMode.Open, FileAccess.Read);
				fileStream2 = new FileStream(AppPath + "\\Resource Files\\" + StarCatalogueBase + ".bin", FileMode.Open, FileAccess.Read);
			}
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			int num10 = (int)fileStream2.Length / num9;
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum(LastAsteroidRecord - FirstAsteroidRecord + 1);
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			for (int i = FirstAsteroidRecord; i <= LastAsteroidRecord; i++)
			{
				flag = false;
				if (DisplayMPOccultations.OccElements.Count > MaximumNumEvents)
				{
					AbortFlag = true;
				}
				if (!astElements[i].AsteroidClass.Contains(AsteroidClass) || astElements[i].EpochYear < 2015)
				{
					continue;
				}
				double diameter_Mean = astElements[i].Diameter_Mean;
				int NumMeasures;
				if (ShowProgressBar)
				{
					CallingForm.pbarSearch.set_Value(i - FirstAsteroidRecord);
					CallingForm.lstResults.get_Items().Clear();
					Label lblAsteroidName = CallingForm.lblAsteroidName;
					NumMeasures = astElements[i].IDNumber;
					((Control)lblAsteroidName).set_Text(NumMeasures + " " + astElements[i].IDName.Trim());
					Application.DoEvents();
				}
				if (!(diameter_Mean >= MinimumAsteroidDiameter || FirstAsteroidRecord == LastAsteroidRecord))
				{
					continue;
				}
				Integrate(astElements, i, StartJD, EndJD, AddNotIntegratedFlag: true);
				for (int j = 0; j < ElementsAt1Day.Count - 1; j++)
				{
					_ = ElementsAt1Day[j].IDNumber;
					_ = ElementsAt1Day[j].IDName;
					int epochYear = ElementsAt1Day[j].EpochYear;
					int epochMonth = ElementsAt1Day[j].EpochMonth;
					double epochDay = ElementsAt1Day[j].EpochDay;
					double meanAnomaly = ElementsAt1Day[j].Meananomaly / (180.0 / Math.PI);
					double perihelion = ElementsAt1Day[j].Perihelion / (180.0 / Math.PI);
					double node = ElementsAt1Day[j].Node / (180.0 / Math.PI);
					double i2 = ElementsAt1Day[j].I / (180.0 / Math.PI);
					double e = ElementsAt1Day[j].E;
					double a = ElementsAt1Day[j].A;
					double q = a * (1.0 - e);
					double h = ElementsAt1Day[j].H0;
					double g_phaseCoeff = ElementsAt1Day[j].G_phaseCoeff;
					double logR_Coeff = ElementsAt1Day[j].LogR_Coeff;
					diameter_Mean = ElementsAt1Day[j].Diameter_Mean;
					_ = ElementsAt1Day[j].Dia_Source;
					double num11 = ElementsAt1Day[j].PeakEphemUncert;
					if (j == 0)
					{
						num3 = 0;
						num2 = 0;
						num = 0;
					}
					if (e > 0.97)
					{
						q = a;
						meanAnomaly = 0.0;
					}
					double num12 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
					double num13 = ((!(e < 0.97)) ? (StartJD + (double)j) : num12);
					Utilities.PositionfromElements(num13, 0.0, 0.0, 0.0, 0.0, num12, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out Magnitude, out var Elongation, out var PhaseAngle);
					Utilities.PositionfromElements(num13 + 1.0, 0.0, 0.0, 0.0, 0.0, num12, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA2, out var Dec2, out RadiusVector, out var AstrometricGeocentricDistance2, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(num13 + 0.99, 0.0, 0.0, 0.0, 0.0, num12, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA3, out var _, out var _, out var _, out var _, out var _, out var _);
					if (Math.Abs(RA2 - RA3) < Math.PI)
					{
						flag3 = RA2 > RA3;
					}
					bool num14 = flag2 != flag3;
					double num15 = (num13 - 2451545.0) / 365.25;
					double num16 = 0.002 + 0.0028 / AstrometricGeocentricDistance;
					double A = RA;
					double B = RA2;
					if (!num14)
					{
						if (flag2 && B < A)
						{
							B += Math.PI * 2.0;
						}
						if (!flag2 && B > A)
						{
							B -= Math.PI * 2.0;
						}
					}
					if (Math.Abs(B - A) < Math.PI)
					{
						if (A > B)
						{
							Utilities.Swap(ref A, ref B);
						}
					}
					else if (A < B)
					{
						Utilities.Swap(ref A, ref B);
					}
					double A2 = Dec;
					double B2 = Dec2;
					if (A2 > B2)
					{
						Utilities.Swap(ref A2, ref B2);
					}
					double num17 = 0.1 + 0.003 / AstrometricGeocentricDistance;
					double num18 = num11 / 8.794143836182533 * AstrometricGeocentricDistance;
					if (num18 > 0.7)
					{
						num18 = 0.7;
					}
					int num19 = 89 - (int)(180.0 / Math.PI * B2 + num17);
					if (B2 < 0.0)
					{
						num19++;
					}
					if (num19 < 0)
					{
						num19 = 0;
					}
					int num20 = 89 - (int)(180.0 / Math.PI * A2 - num17);
					if (A2 < 0.0)
					{
						num20++;
					}
					if (num20 < 0)
					{
						num20 = 0;
					}
					int num21 = 0;
					for (int k = num19; k <= num20; k++)
					{
						int num22 = 361 * k;
						int num23 = (int)Math.Floor(A * (180.0 / Math.PI) - 0.02);
						if (num23 < 0)
						{
							num23 += 360;
						}
						if (num23 > 360)
						{
							num23 -= 360;
						}
						int num24 = (int)Math.Ceiling(B * (180.0 / Math.PI) + 0.02);
						if (num24 > 360)
						{
							num24 -= 360;
						}
						if (num24 < 0)
						{
							num24 += 360;
						}
						fileStream.Seek(num22 * 4, SeekOrigin.Begin);
						array[0] = binaryReader.ReadInt32() - num9;
						if (array[0] < 0)
						{
							array[0] = 0;
						}
						fileStream.Seek((num22 + num23) * 4, SeekOrigin.Begin);
						array[1] = binaryReader.ReadInt32() - 1;
						if (array[1] < 0)
						{
							array[1] = 0;
						}
						fileStream.Seek((num22 + num24) * 4, SeekOrigin.Begin);
						array[2] = binaryReader.ReadInt32();
						fileStream.Seek((num22 + 360) * 4, SeekOrigin.Begin);
						array[3] = binaryReader.ReadInt32();
						int num25 = array[1];
						int num26 = array[2];
						double num27 = 0.0;
						if (num25 >= num26)
						{
							num25 = array[0];
							num26 = array[2];
							num27 = Math.PI * 2.0;
						}
						double num28 = B;
						double num29 = A - num27;
						do
						{
							IL_08a2:
							if (AbortFlag)
							{
								binaryReader.Close();
								binaryReader2.Close();
								if (ShowProgressBar)
								{
									((Control)CallingForm.pbarSearch).set_Visible(false);
									((Control)CallingForm.cmdCompute).set_Visible(true);
									CallingForm.SearchCancelled = true;
								}
								return;
							}
							if (num25 < num10)
							{
								Tycho2.ReadNext(fileStream2, binaryReader2, num25);
								double rA;
								double num30 = (rA = Tycho2.RA);
								double num31 = (num8 = Tycho2.Dec);
								if (rA > num29 - num16 && rA < num28 + num16 && num8 > A2 - num16 && num8 < B2 + num16)
								{
									double num32 = num15 * Tycho2.PMRA;
									double num33 = num15 * Tycho2.PMDec;
									double piStar = Tycho2.Parallax_asec / 3600.0 / (180.0 / Math.PI);
									double magV = Tycho2.MagV;
									double magB = Tycho2.MagB;
									double num34 = -2.5 * Math.Log10(Math.Pow(10.0, magV / -2.5) + Math.Pow(10.0, Magnitude / -2.5));
									if ((magV <= LimitingStarMagForSearch) & (Magnitude - num34 >= MagDropLimitForAsteroidSearches))
									{
										int starNumber = Tycho2.StarNumber;
										if (starNumber != num && starNumber != num2 && starNumber != num3)
										{
											rA += num32;
											num8 += num33;
											flag4 = true;
											if (LimitSearchToKepler2)
											{
												if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
												{
													Kepler2.Initialise_Kepler2_ForAsteroids();
												}
												flag4 = Kepler2.StarInKepler2(rA * (180.0 / Math.PI), num8 * (180.0 / Math.PI));
											}
											if (flag4)
											{
												BessellianElements(rA, num8, RA, Dec, AstrometricGeocentricDistance, out var X, out var Y);
												BessellianElements(rA, num8, RA2, Dec2, AstrometricGeocentricDistance2, out var X2, out var Y2);
												double num35 = X2 - X;
												double num36 = Y2 - Y;
												double num37 = Math.Sqrt(num35 * num35 + num36 * num36);
												double num38 = X * num35 + Y * num36;
												double value = (X * num36 - Y * num35) / num37;
												double num39 = Math.Floor((0.0 - num38) / num37 / num37 * 24.0);
												double num40 = diameter_Mean / num37 * 86400.0 / 6378.137;
												if (Math.Abs(value) < 10.0 + num18 + ExpandedSearchDistInEarthRadii && num39 > -8.0 && num39 < 32.0 && num40 > MinimumDuration)
												{
													string errorBasis;
													if (num11 == 0.0)
													{
														num11 = 0.2;
														errorBasis = "Star+Assumed";
													}
													else
													{
														errorBasis = "Star+PeakEphemUncert";
													}
													num7 = 90.0;
													string starID = Tycho2.StarID;
													double num41 = Tycho2.ErrorRA_arcsecs(2000.0 + num15);
													double num42 = Tycho2.ErrorDec_arcsecs(2000.0 + num15);
													num5 = Math.Sqrt(num11 * num11 + num41 * num41);
													num6 = Math.Sqrt(num11 * num11 + num42 * num42);
													num7 = 90.0;
													string Basis;
													bool InvalidDiameter;
													double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num30 * (180.0 / Math.PI) / 15.0, num31 * (180.0 / Math.PI), magV, magB, 0.0, out Basis, out NumMeasures, out InvalidDiameter);
													MinorPlanetPrediction(num13, num39, j, starID, rA, num8, piStar, magV, magB, 20.0, num5, num6, num7, 0.0, 0.0, errorBasis, num18 + ExpandedSearchDistInEarthRadii, starDiaMAS, out flag, UseDefaultErrorSettings, AutoSaveSearch, OutputFileName);
													num4++;
													if (flag)
													{
														num3 = num2;
														num2 = num;
														num = starNumber;
														num21++;
													}
												}
											}
										}
									}
								}
								num25++;
								if (num25 <= num26)
								{
									goto IL_08a2;
								}
							}
							if (num27 == 0.0)
							{
								break;
							}
							num27 = 0.0;
							num25 = array[1];
							num26 = array[3];
							num28 += Math.PI * 2.0;
							num29 += Math.PI * 2.0;
						}
						while (num27 >= 0.0);
					}
					if (num21 == 0)
					{
						num3 = num2;
						num2 = num;
						num = 0;
					}
				}
			}
			binaryReader.Close();
			binaryReader2.Close();
			if (ShowProgressBar)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		internal static void MinorPlanetSearch_UCAC4(double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDuration, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double Magnitude = 20.0;
			bool flag2 = false;
			bool flag3 = false;
			_ = new char[141];
			bool flag4 = false;
			double num8 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			_ = new byte[12];
			bool flag5 = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			List<AsteroidElements> astElements;
			if (!UseUserElementFile)
			{
				astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else
			{
				astElements = Elements.UserAsteroids.AstElements;
				Elements.UserAsteroids.SourceFile = UserElementsFile;
				if ((astElements.Count < 1) | CallingForm.chkHorizons.get_Checked())
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
			}
			UCAC4.InitialiseUCAC4();
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum(LastAsteroidRecord - FirstAsteroidRecord + 1);
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			for (int i = FirstAsteroidRecord; i <= LastAsteroidRecord; i++)
			{
				flag = false;
				if (DisplayMPOccultations.OccElements.Count > MaximumNumEvents)
				{
					AbortFlag = true;
				}
				if (!astElements[i].AsteroidClass.Contains(AsteroidClass) || astElements[i].EpochYear < 2015)
				{
					continue;
				}
				double diameter_Mean = astElements[i].Diameter_Mean;
				int NumMeasures;
				if (ShowProgressBar)
				{
					CallingForm.pbarSearch.set_Value(i - FirstAsteroidRecord);
					CallingForm.lstResults.get_Items().Clear();
					Label lblAsteroidName = CallingForm.lblAsteroidName;
					NumMeasures = astElements[i].IDNumber;
					((Control)lblAsteroidName).set_Text(NumMeasures + " " + astElements[i].IDName.Trim());
					Application.DoEvents();
				}
				if (!(diameter_Mean >= MinimumAsteroidDiameter || FirstAsteroidRecord == LastAsteroidRecord))
				{
					continue;
				}
				Integrate(astElements, i, StartJD, EndJD, AddNotIntegratedFlag: true);
				for (int j = 0; j < ElementsAt1Day.Count - 1; j++)
				{
					_ = ElementsAt1Day[j].IDNumber;
					_ = ElementsAt1Day[j].IDName;
					int epochYear = ElementsAt1Day[j].EpochYear;
					int epochMonth = ElementsAt1Day[j].EpochMonth;
					double epochDay = ElementsAt1Day[j].EpochDay;
					double meanAnomaly = ElementsAt1Day[j].Meananomaly / (180.0 / Math.PI);
					double perihelion = ElementsAt1Day[j].Perihelion / (180.0 / Math.PI);
					double node = ElementsAt1Day[j].Node / (180.0 / Math.PI);
					double i2 = ElementsAt1Day[j].I / (180.0 / Math.PI);
					double e = ElementsAt1Day[j].E;
					double a = ElementsAt1Day[j].A;
					double q = a * (1.0 - e);
					double h = ElementsAt1Day[j].H0;
					double g_phaseCoeff = ElementsAt1Day[j].G_phaseCoeff;
					double logR_Coeff = ElementsAt1Day[j].LogR_Coeff;
					diameter_Mean = ElementsAt1Day[j].Diameter_Mean;
					_ = ElementsAt1Day[j].Dia_Source;
					double num9 = ElementsAt1Day[j].PeakEphemUncert;
					if (j == 0)
					{
						num3 = 0;
						num2 = 0;
						num = 0;
					}
					if (e > 0.97)
					{
						q = a;
						meanAnomaly = 0.0;
					}
					double num10 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
					double num11 = ((!(e < 0.97)) ? (StartJD + (double)j) : num10);
					Utilities.PositionfromElements(num11, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out Magnitude, out var Elongation, out var PhaseAngle);
					Utilities.PositionfromElements(num11 + 1.0, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA2, out var Dec2, out RadiusVector, out var AstrometricGeocentricDistance2, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(num11 + 0.99, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA3, out var _, out var _, out var _, out var _, out var _, out var _);
					if (Math.Abs(RA2 - RA3) < Math.PI)
					{
						flag3 = RA2 > RA3;
					}
					bool num12 = flag2 != flag3;
					double num13 = (num11 - 2451545.0) / 365.25;
					double num14 = 0.002 + 0.0028 / AstrometricGeocentricDistance;
					double A = RA;
					double B = RA2;
					if (!num12)
					{
						if (flag2 && B < A)
						{
							B += Math.PI * 2.0;
						}
						if (!flag2 && B > A)
						{
							B -= Math.PI * 2.0;
						}
					}
					if (Math.Abs(B - A) < Math.PI)
					{
						if (A > B)
						{
							Utilities.Swap(ref A, ref B);
						}
					}
					else if (A < B)
					{
						Utilities.Swap(ref A, ref B);
					}
					double A2 = Dec;
					double B2 = Dec2;
					if (A2 > B2)
					{
						Utilities.Swap(ref A2, ref B2);
					}
					double num15 = 0.1 + 0.003 / AstrometricGeocentricDistance;
					double num16 = num9 / 8.794143836182533 * AstrometricGeocentricDistance;
					if (num16 > 0.7)
					{
						num16 = 0.7;
					}
					int num17 = (int)(451.0 + 900.0 / Math.PI * B2 + num15);
					if (num17 < 1)
					{
						num17 = 1;
					}
					int num18 = (int)(451.0 + 900.0 / Math.PI * A2 - num15);
					if (num18 < 1)
					{
						num18 = 1;
					}
					int num19 = 0;
					for (int k = num18; k <= num17 && k <= 900; k++)
					{
						int num20 = (int)Math.Floor(A * (180.0 / Math.PI) * 4.0 - 0.02);
						if (num20 < 0)
						{
							num20 += 1440;
						}
						if (num20 > 1439)
						{
							num20 -= 1440;
						}
						int num21 = (int)Math.Ceiling(B * (180.0 / Math.PI) * 4.0 + 0.02);
						if (num21 < 0)
						{
							num21 += 1440;
						}
						if (num21 > 1439)
						{
							num21 -= 1440;
						}
						UCAC4.Open_UCAC4_Catalogue(k);
						array[0] = 0;
						UCAC4.GetUCAC4IndexAndBin(k, num20, out var StartRecordNum, out var NumInBin);
						array[1] = StartRecordNum;
						UCAC4.GetUCAC4IndexAndBin(k, num21, out StartRecordNum, out NumInBin);
						array[2] = StartRecordNum + NumInBin;
						UCAC4.GetUCAC4IndexAndBin(k, 1439, out StartRecordNum, out NumInBin);
						array[3] = StartRecordNum + NumInBin;
						int num22 = array[1];
						int num23 = array[2];
						double num24 = 0.0;
						if (num22 >= num23)
						{
							num22 = array[0];
							num23 = array[2];
							num24 = Math.PI * 2.0;
						}
						double num25 = B;
						double num26 = A - num24;
						while (true)
						{
							if (num22 < num23)
							{
								if (AbortFlag)
								{
									UCAC4.Close_UCAC4_Catalogue();
									if (ShowProgressBar)
									{
										((Control)CallingForm.pbarSearch).set_Visible(false);
										((Control)CallingForm.cmdCompute).set_Visible(true);
										CallingForm.SearchCancelled = true;
									}
									return;
								}
								int num27 = num22 + 1;
								if (num27 != num && num27 != num2 && num27 != num3)
								{
									UCAC4.Read_UCAC4_entry(num22, UseHipparcosForParallax: false);
									if (flag4 | !UCAC4.DoubtfulObject)
									{
										double rA;
										double num28 = (rA = UCAC4.RA);
										double num29 = (num8 = UCAC4.Dec);
										if (rA > num26 - num14 && rA < num25 + num14 && num8 > A2 - num14 && num8 < B2 + num14)
										{
											double mag = UCAC4.Mag;
											double num30 = num13 * UCAC4.PM_ra;
											double num31 = num13 * UCAC4.PM_dec;
											double num32 = -2.5 * Math.Log10(Math.Pow(10.0, mag / -2.5) + Math.Pow(10.0, Magnitude / -2.5));
											if ((mag <= LimitingStarMagForSearch) & (Magnitude - num32 >= MagDropLimitForAsteroidSearches))
											{
												rA += num30;
												num8 += num31;
												double piStar = UCAC4.Parallax_mas / 1000.0 / 3600.0 / (180.0 / Math.PI);
												flag5 = true;
												if (LimitSearchToKepler2)
												{
													if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
													{
														Kepler2.Initialise_Kepler2_ForAsteroids();
													}
													flag5 = Kepler2.StarInKepler2(rA * (180.0 / Math.PI), num8 * (180.0 / Math.PI));
												}
												if (flag5)
												{
													BessellianElements(rA, num8, RA, Dec, AstrometricGeocentricDistance, out var X, out var Y);
													BessellianElements(rA, num8, RA2, Dec2, AstrometricGeocentricDistance2, out var X2, out var Y2);
													double num33 = X2 - X;
													double num34 = Y2 - Y;
													double num35 = Math.Sqrt(num33 * num33 + num34 * num34);
													double num36 = X * num33 + Y * num34;
													double value = (X * num34 - Y * num33) / num35;
													double num37 = Math.Floor((0.0 - num36) / num35 / num35 * 24.0);
													double num38 = diameter_Mean / num35 * 86400.0 / 6378.137;
													if (Math.Abs(value) < 10.0 + num16 + ExpandedSearchDistInEarthRadii && num37 > -8.0 && num37 < 32.0 && num38 > MinimumDuration)
													{
														string errorBasis;
														if (num9 == 0.0)
														{
															num9 = 0.2;
															errorBasis = "Star+Assumed";
														}
														else
														{
															errorBasis = "Star+PeakEphemUncert";
														}
														double num39 = UCAC4.ErrorRA_arcsecs(2000.0 + num13);
														double num40 = UCAC4.ErrorDec_arcsecs(2000.0 + num13);
														num5 = Math.Sqrt(num9 * num9 + num39 * num39);
														num6 = Math.Sqrt(num9 * num9 + num40 * num40);
														num7 = 90.0;
														string uCACnumber = UCAC4.UCACnumber;
														double magB = UCAC4.MagB;
														double magR = UCAC4.MagR;
														string Basis;
														bool InvalidDiameter;
														double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num28 * (180.0 / Math.PI) / 15.0, num29 * (180.0 / Math.PI), mag, magB, magR, out Basis, out NumMeasures, out InvalidDiameter);
														MinorPlanetPrediction(num11, num37, j, uCACnumber, rA, num8, piStar, mag, magB, magR, num5, num6, num7, 0.0, 0.0, errorBasis, num16 + ExpandedSearchDistInEarthRadii, starDiaMAS, out flag, UseDefaultErrorSettings, AutoSaveSearch, OutputFileName);
														num4++;
														if (flag)
														{
															num3 = num2;
															num2 = num;
															num = num27;
															num19++;
														}
													}
												}
											}
										}
									}
								}
								num22++;
							}
							else
							{
								if (num24 == 0.0)
								{
									break;
								}
								num24 = 0.0;
								num22 = array[1];
								num23 = array[3];
								num25 += Math.PI * 2.0;
								num26 += Math.PI * 2.0;
								if (!(num24 >= 0.0))
								{
									break;
								}
							}
						}
						UCAC4.Close_UCAC4_Catalogue();
					}
					if (num19 == 0)
					{
						num3 = num2;
						num2 = num;
						num = 0;
					}
				}
			}
			if (ShowProgressBar)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		internal static void MinorPlanetSearch_PPMXL(double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDuration, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double Magnitude = 20.0;
			bool flag2 = false;
			bool flag3 = false;
			_ = new char[141];
			double num8 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			_ = new byte[12];
			bool flag4 = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			List<AsteroidElements> astElements;
			if (!UseUserElementFile)
			{
				astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else
			{
				astElements = Elements.UserAsteroids.AstElements;
				Elements.UserAsteroids.SourceFile = UserElementsFile;
				if ((astElements.Count < 1) | CallingForm.chkHorizons.get_Checked())
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
			}
			PPMXL.InitialisePPMXL();
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum(LastAsteroidRecord - FirstAsteroidRecord + 1);
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			for (int i = FirstAsteroidRecord; i <= LastAsteroidRecord; i++)
			{
				flag = false;
				if (DisplayMPOccultations.OccElements.Count > MaximumNumEvents)
				{
					AbortFlag = true;
				}
				if (!astElements[i].AsteroidClass.Contains(AsteroidClass) || astElements[i].EpochYear < 2015)
				{
					continue;
				}
				double diameter_Mean = astElements[i].Diameter_Mean;
				int NumMeasures;
				if (ShowProgressBar)
				{
					CallingForm.pbarSearch.set_Value(i - FirstAsteroidRecord);
					CallingForm.lstResults.get_Items().Clear();
					Label lblAsteroidName = CallingForm.lblAsteroidName;
					NumMeasures = astElements[i].IDNumber;
					((Control)lblAsteroidName).set_Text(NumMeasures + " " + astElements[i].IDName.Trim());
					Application.DoEvents();
				}
				if (!(diameter_Mean >= MinimumAsteroidDiameter || FirstAsteroidRecord == LastAsteroidRecord))
				{
					continue;
				}
				Integrate(astElements, i, StartJD, EndJD, AddNotIntegratedFlag: true);
				for (int j = 0; j < ElementsAt1Day.Count - 1; j++)
				{
					_ = ElementsAt1Day[j].IDNumber;
					_ = ElementsAt1Day[j].IDName;
					int epochYear = ElementsAt1Day[j].EpochYear;
					int epochMonth = ElementsAt1Day[j].EpochMonth;
					double epochDay = ElementsAt1Day[j].EpochDay;
					double meanAnomaly = ElementsAt1Day[j].Meananomaly / (180.0 / Math.PI);
					double perihelion = ElementsAt1Day[j].Perihelion / (180.0 / Math.PI);
					double node = ElementsAt1Day[j].Node / (180.0 / Math.PI);
					double i2 = ElementsAt1Day[j].I / (180.0 / Math.PI);
					double e = ElementsAt1Day[j].E;
					double a = ElementsAt1Day[j].A;
					double q = a * (1.0 - e);
					double h = ElementsAt1Day[j].H0;
					double g_phaseCoeff = ElementsAt1Day[j].G_phaseCoeff;
					double logR_Coeff = ElementsAt1Day[j].LogR_Coeff;
					diameter_Mean = ElementsAt1Day[j].Diameter_Mean;
					_ = ElementsAt1Day[j].Dia_Source;
					double num9 = ElementsAt1Day[j].PeakEphemUncert;
					if (j == 0)
					{
						num3 = 0;
						num2 = 0;
						num = 0;
					}
					if (e > 0.97)
					{
						q = a;
						meanAnomaly = 0.0;
					}
					double num10 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
					double num11 = ((!(e < 0.97)) ? (StartJD + (double)j) : num10);
					Utilities.PositionfromElements(num11, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out Magnitude, out var Elongation, out var PhaseAngle);
					Utilities.PositionfromElements(num11 + 1.0, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA2, out var Dec2, out RadiusVector, out var AstrometricGeocentricDistance2, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(num11 + 0.99, 0.0, 0.0, 0.0, 0.0, num10, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA3, out var _, out var _, out var _, out var _, out var _, out var _);
					if (Math.Abs(RA2 - RA3) < Math.PI)
					{
						flag3 = RA2 > RA3;
					}
					bool num12 = flag2 != flag3;
					double num13 = (num11 - 2451545.0) / 365.25;
					double num14 = 0.002 + 0.0028 / AstrometricGeocentricDistance;
					double A = RA;
					double B = RA2;
					if (!num12)
					{
						if (flag2 && B < A)
						{
							B += Math.PI * 2.0;
						}
						if (!flag2 && B > A)
						{
							B -= Math.PI * 2.0;
						}
					}
					if (Math.Abs(B - A) < Math.PI)
					{
						if (A > B)
						{
							Utilities.Swap(ref A, ref B);
						}
					}
					else if (A < B)
					{
						Utilities.Swap(ref A, ref B);
					}
					double A2 = Dec;
					double B2 = Dec2;
					if (A2 > B2)
					{
						Utilities.Swap(ref A2, ref B2);
					}
					double num15 = 0.1 + 0.003 / AstrometricGeocentricDistance;
					double num16 = num9 / 8.794143836182533 * AstrometricGeocentricDistance;
					if (num16 > 0.7)
					{
						num16 = 0.7;
					}
					int num17 = (int)(361.0 + 720.0 / Math.PI * B2 + num15);
					if (num17 < 0)
					{
						num17 = 0;
					}
					int num18 = (int)(361.0 + 720.0 / Math.PI * A2 - num15);
					if (num18 < 0)
					{
						num18 = 0;
					}
					int num19 = 0;
					for (int k = num18; k <= num17 && k <= 720; k++)
					{
						int num20 = (int)Math.Floor(A * (180.0 / Math.PI) * 5.0 - 0.02);
						if (num20 < 0)
						{
							num20 += 1800;
						}
						if (num20 > 1800)
						{
							num20 -= 1800;
						}
						int num21 = (int)Math.Ceiling(B * (180.0 / Math.PI) * 5.0 + 0.02);
						if (num21 < 0)
						{
							num21 += 1800;
						}
						if (num21 > 1800)
						{
							num21 -= 1800;
						}
						PPMXL.Open_PPMXL_Catalogue(k);
						array[0] = PPMXL.GetPPMXLIndexValue(k, 0);
						array[1] = PPMXL.GetPPMXLIndexValue(k, num20);
						array[2] = PPMXL.GetPPMXLIndexValue(k, num21);
						array[3] = PPMXL.GetPPMXLIndexValue(k, 1800);
						int num22 = array[1];
						int num23 = array[2];
						double num24 = 0.0;
						if (num22 >= num23)
						{
							num22 = array[0];
							num23 = array[2];
							num24 = Math.PI * 2.0;
						}
						double num25 = B;
						double num26 = A - num24;
						while (true)
						{
							if (num22 < num23)
							{
								if (AbortFlag)
								{
									PPMXL.Close_PPMXL_Catalogue();
									if (ShowProgressBar)
									{
										((Control)CallingForm.pbarSearch).set_Visible(false);
										((Control)CallingForm.cmdCompute).set_Visible(true);
										CallingForm.SearchCancelled = true;
									}
									return;
								}
								int num27 = num22 + 1;
								if (num27 != num && num27 != num2 && num27 != num3)
								{
									PPMXL.Read_PPMXL_entry(num22);
									double rA_J;
									double num28 = (rA_J = PPMXL.RA_J2000);
									double num29 = (num8 = PPMXL.Dec_J2000);
									if (rA_J > num26 - num14 && rA_J < num25 + num14 && num8 > A2 - num14 && num8 < B2 + num14)
									{
										double mag = PPMXL.mag;
										double num30 = num13 * PPMXL.PM_RA;
										double num31 = num13 * PPMXL.PM_Dec;
										double num32 = -2.5 * Math.Log10(Math.Pow(10.0, mag / -2.5) + Math.Pow(10.0, Magnitude / -2.5));
										if ((mag <= LimitingStarMagForSearch) & (Magnitude - num32 >= MagDropLimitForAsteroidSearches))
										{
											rA_J += num30;
											num8 += num31;
											flag4 = true;
											if (LimitSearchToKepler2)
											{
												if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
												{
													Kepler2.Initialise_Kepler2_ForAsteroids();
												}
												flag4 = Kepler2.StarInKepler2(rA_J * (180.0 / Math.PI), num8 * (180.0 / Math.PI));
											}
											if (flag4)
											{
												BessellianElements(rA_J, num8, RA, Dec, AstrometricGeocentricDistance, out var X, out var Y);
												BessellianElements(rA_J, num8, RA2, Dec2, AstrometricGeocentricDistance2, out var X2, out var Y2);
												double num33 = X2 - X;
												double num34 = Y2 - Y;
												double num35 = Math.Sqrt(num33 * num33 + num34 * num34);
												double num36 = X * num33 + Y * num34;
												double value = (X * num34 - Y * num33) / num35;
												double num37 = Math.Floor((0.0 - num36) / num35 / num35 * 24.0);
												double num38 = diameter_Mean / num35 * 86400.0 / 6378.137;
												if (Math.Abs(value) < 10.0 + num16 + ExpandedSearchDistInEarthRadii && num37 > -8.0 && num37 < 32.0 && num38 > MinimumDuration)
												{
													string errorBasis;
													if (num9 == 0.0)
													{
														num9 = 0.2;
														errorBasis = "Star+Assumed";
													}
													else
													{
														errorBasis = "Star+PeakEphemUncert";
													}
													double num39 = PPMXL.ErrorRA_arcsecs(2000.0 + num13);
													double num40 = PPMXL.ErrorDec_arcsecs(2000.0 + num13);
													num5 = Math.Sqrt(num9 * num9 + num39 * num39);
													num6 = Math.Sqrt(num9 * num9 + num40 * num40);
													num7 = 90.0;
													string pPMXnumber = PPMXL.PPMXnumber;
													double mag_B = PPMXL.mag_B;
													double mag_R = PPMXL.mag_R;
													string Basis;
													bool InvalidDiameter;
													double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num28 * (180.0 / Math.PI) / 15.0, num29 * (180.0 / Math.PI), mag, mag_B, mag_R, out Basis, out NumMeasures, out InvalidDiameter);
													MinorPlanetPrediction(num11, num37, j, pPMXnumber, rA_J, num8, 0.0, mag, mag_B, mag_R, num5, num6, num7, 0.0, 0.0, errorBasis, num16 + ExpandedSearchDistInEarthRadii, starDiaMAS, out flag, UseDefaultErrorSettings, AutoSaveSearch, OutputFileName);
													num4++;
													if (flag)
													{
														num3 = num2;
														num2 = num;
														num = num27;
														num19++;
													}
												}
											}
										}
									}
								}
								num22++;
							}
							else
							{
								if (num24 == 0.0)
								{
									break;
								}
								num24 = 0.0;
								num22 = array[1];
								num23 = array[3];
								num25 += Math.PI * 2.0;
								num26 += Math.PI * 2.0;
								if (!(num24 >= 0.0))
								{
									break;
								}
							}
						}
						PPMXL.Close_PPMXL_Catalogue();
					}
					if (num19 == 0)
					{
						num3 = num2;
						num2 = num;
						num = 0;
					}
				}
			}
			PPMXL.ReleasePPMXL();
			if (ShowProgressBar)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		internal static void MinorPlanetSearch_UserStars(double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, string AsteroidClass, double LimitingStarMagForSearch, double MinimumDuration, double ExpandedSearchDistInEarthRadii, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, string UserStarCatFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar, bool UseKnownErrors, double KnownErrorEllipseMajor, double KnownErrorEllipseMinor, double KnownErrorEllipsePA, double KnownErrorSigma_arcsec, double KnownError_as_IncreaseInPathWidth)
		{
			bool flag = false;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double Magnitude = 20.0;
			int noGaiaPM = -1;
			int gaiaUCAC4PM = -1;
			int duplicateSource = -1;
			bool flag2 = false;
			bool flag3 = false;
			_ = new char[141];
			double num6 = 0.0;
			double num7 = 2000.0;
			double num8 = 0.0;
			double radialVelocity_KmSec = 0.0;
			double starReliability = -1.0;
			_ = new string[5];
			List<AsteroidElements> astElements;
			if (!UseUserElementFile)
			{
				astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else
			{
				astElements = Elements.UserAsteroids.AstElements;
				astElements.Clear();
				Elements.UserAsteroids.SourceFile = UserElementsFile;
				if (astElements.Count < 1)
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
				if (CallingForm != null && CallingForm.chkHorizons.get_Checked())
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
			}
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			string text;
			using (StreamReader streamReader = new StreamReader(UserStarCatFile))
			{
				text = streamReader.ReadLine();
			}
			string starCatName;
			double num9;
			double muRA_Annual_Rad;
			double num10;
			double muDec_Annual_Rad;
			double num11;
			double magV;
			double magR;
			double magB;
			if (text.Substring(21).Contains(","))
			{
				string[] array = text.Split(new char[1] { ',' });
				starCatName = array[0].Trim();
				num7 = double.Parse(array[1]);
				num9 = (double.Parse(array[2]) + double.Parse(array[3]) / 60.0 + double.Parse(array[4]) / 3600.0) * 15.0 / (180.0 / Math.PI);
				muRA_Annual_Rad = double.Parse(array[5]) / 3600.0 * 15.0 / (180.0 / Math.PI);
				num10 = (double.Parse(array[6].Replace("-", "").Replace("+", "")) + double.Parse(array[7]) / 60.0 + double.Parse(array[8]) / 3600.0) / (180.0 / Math.PI);
				if (array[6].Contains("-"))
				{
					num10 = 0.0 - num10;
				}
				muDec_Annual_Rad = double.Parse(array[9]) / 3600.0 / (180.0 / Math.PI);
				num11 = double.Parse(array[10]) / 3600.0 / (180.0 / Math.PI);
				radialVelocity_KmSec = double.Parse(array[11]);
				magV = double.Parse(array[12]);
				magR = double.Parse(array[13]);
				magB = double.Parse(array[14]);
				if (array.Length > 15 && array[15].Trim() != "")
				{
					starReliability = double.Parse(array[15]);
				}
				if (array.Length > 16 && array[16].Trim() != "")
				{
					duplicateSource = int.Parse(array[16]);
				}
				if (array.Length > 17 && array[17].Trim() != "")
				{
					noGaiaPM = int.Parse(array[17]);
				}
				if (array.Length > 18 && array[18].Trim() != "")
				{
					gaiaUCAC4PM = int.Parse(array[18]);
				}
			}
			else
			{
				starCatName = text.Substring(0, 20).Trim();
				num9 = (double.Parse(text.Substring(21, 2)) + double.Parse(text.Substring(23, 2)) / 60.0 + double.Parse(text.Substring(25, 7)) / 3600.0) * 15.0 / (180.0 / Math.PI);
				muRA_Annual_Rad = double.Parse(text.Substring(32, 8)) / 3600.0 * 15.0 / (180.0 / Math.PI);
				num10 = (double.Parse(text.Substring(43, 2)) + double.Parse(text.Substring(45, 2)) / 60.0 + double.Parse(text.Substring(47, 6)) / 3600.0) / (180.0 / Math.PI);
				if (text.Substring(42, 1) == "-")
				{
					num6 = 0.0 - num6;
				}
				muDec_Annual_Rad = double.Parse(text.Substring(53, 7)) / 3600.0 / (180.0 / Math.PI);
				num11 = double.Parse(text.Substring(62, 6)) / 3600.0 / (180.0 / Math.PI);
				magV = double.Parse(text.Substring(69, 5));
				magR = 20.0;
				magB = double.Parse(text.Substring(75, 5));
			}
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum(LastAsteroidRecord - FirstAsteroidRecord + 1);
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			for (int i = FirstAsteroidRecord; i <= LastAsteroidRecord; i++)
			{
				flag = false;
				if (DisplayMPOccultations.OccElements.Count > MaximumNumEvents)
				{
					AbortFlag = true;
				}
				if (!astElements[i].AsteroidClass.Contains(AsteroidClass) || astElements[i].EpochYear < 2015)
				{
					continue;
				}
				double diameter_Mean = astElements[i].Diameter_Mean;
				int NumMeasures;
				if (ShowProgressBar)
				{
					CallingForm.pbarSearch.set_Value(i - FirstAsteroidRecord);
					CallingForm.lstResults.get_Items().Clear();
					Label lblAsteroidName = CallingForm.lblAsteroidName;
					NumMeasures = astElements[i].IDNumber;
					((Control)lblAsteroidName).set_Text(NumMeasures + " " + astElements[i].IDName.Trim());
					Application.DoEvents();
				}
				if (AbortFlag)
				{
					((Control)CallingForm.pbarSearch).set_Visible(false);
					((Control)CallingForm.cmdCompute).set_Visible(true);
					CallingForm.SearchCancelled = true;
					return;
				}
				if (!(diameter_Mean >= MinimumAsteroidDiameter || FirstAsteroidRecord == LastAsteroidRecord))
				{
					continue;
				}
				Integrate(astElements, i, StartJD, EndJD, astElements.Count > 1);
				for (int j = 0; j < ElementsAt1Day.Count - 1; j++)
				{
					_ = ElementsAt1Day[j].IDNumber;
					_ = ElementsAt1Day[j].IDName;
					int epochYear = ElementsAt1Day[j].EpochYear;
					int epochMonth = ElementsAt1Day[j].EpochMonth;
					double epochDay = ElementsAt1Day[j].EpochDay;
					double meanAnomaly = ElementsAt1Day[j].Meananomaly / (180.0 / Math.PI);
					double perihelion = ElementsAt1Day[j].Perihelion / (180.0 / Math.PI);
					double node = ElementsAt1Day[j].Node / (180.0 / Math.PI);
					double i2 = ElementsAt1Day[j].I / (180.0 / Math.PI);
					double e = ElementsAt1Day[j].E;
					double a = ElementsAt1Day[j].A;
					double q = a * (1.0 - e);
					double h = ElementsAt1Day[j].H0;
					double g_phaseCoeff = ElementsAt1Day[j].G_phaseCoeff;
					double logR_Coeff = ElementsAt1Day[j].LogR_Coeff;
					diameter_Mean = ElementsAt1Day[j].Diameter_Mean;
					_ = ElementsAt1Day[j].Dia_Source;
					double num12 = ElementsAt1Day[j].PeakEphemUncert;
					if (e > 0.97)
					{
						q = a;
						meanAnomaly = 0.0;
					}
					double num13 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
					double num14 = ((!(e < 0.97)) ? (StartJD + (double)j) : num13);
					Utilities.PositionfromElements(num14, 0.0, 0.0, 0.0, 0.0, num13, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out Magnitude, out var Elongation, out var PhaseAngle);
					Utilities.PositionfromElements(num14 + 1.0, 0.0, 0.0, 0.0, 0.0, num13, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA2, out var Dec2, out RadiusVector, out var AstrometricGeocentricDistance2, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(num14 + 0.99, 0.0, 0.0, 0.0, 0.0, num13, meanAnomaly, q, e, perihelion, node, i2, h, g_phaseCoeff, logR_Coeff, 0.0, out var RA3, out var _, out var _, out var _, out var _, out var _, out var _);
					if (Math.Abs(RA2 - RA3) < Math.PI)
					{
						flag3 = RA2 > RA3;
					}
					bool num15 = flag2 != flag3;
					num8 = (num14 - 2451545.0) / 365.25;
					double num16 = 0.002 + 0.0028 / AstrometricGeocentricDistance;
					double RA4 = num9;
					num6 = num10;
					Utilities.ProperMotion(ref RA4, ref num6, muRA_Annual_Rad, muDec_Annual_Rad, num11, radialVelocity_KmSec, num8 + 2000.0 - num7);
					num = RA;
					num2 = RA2;
					if (!num15)
					{
						if (flag2 && num2 < num)
						{
							num2 += Math.PI * 2.0;
						}
						if (!flag2 && num2 > num)
						{
							num2 -= Math.PI * 2.0;
						}
					}
					if (Math.Abs(num2 - num) < Math.PI)
					{
						if (num > num2)
						{
							Utilities.Swap(ref num, ref num2);
						}
					}
					else
					{
						if (num < num2)
						{
							Utilities.Swap(ref num, ref num2);
						}
						if (RA4 < 0.2 && num > 6.0 && num2 < 0.3)
						{
							num -= Math.PI * 2.0;
						}
						if (RA4 > 6.0 && num > 6.0 && num2 < 0.3)
						{
							num2 += Math.PI * 2.0;
						}
					}
					double A = Dec;
					double B = Dec2;
					if (A > B)
					{
						Utilities.Swap(ref A, ref B);
					}
					_ = 0.003 / AstrometricGeocentricDistance;
					double num17 = num12 / 8.794143836182533 * AstrometricGeocentricDistance;
					if (num17 > 0.7)
					{
						num17 = 0.7;
					}
					if (RA4 > num - num16 && RA4 < num2 + num16 && num6 > A - num16 && num6 < B + num16)
					{
						BessellianElements(RA4, num6, RA, Dec, AstrometricGeocentricDistance, out var X, out var Y);
						BessellianElements(RA4, num6, RA2, Dec2, AstrometricGeocentricDistance2, out var X2, out var Y2);
						double num18 = X2 - X;
						double num19 = Y2 - Y;
						double num20 = Math.Sqrt(num18 * num18 + num19 * num19);
						double num21 = X * num18 + Y * num19;
						double value = (X * num19 - Y * num18) / num20;
						double num22 = Math.Floor((0.0 - num21) / num20 / num20 * 24.0);
						double num23 = diameter_Mean / num20 * 86400.0 / 6378.137;
						if (Math.Abs(value) < 10.0 + num17 + ExpandedSearchDistInEarthRadii && num22 > -8.0 && num22 < 32.0 && num23 > MinimumDuration)
						{
							string text2 = "";
							if (UseKnownErrors)
							{
								num3 = KnownErrorEllipseMajor;
								num4 = KnownErrorEllipseMinor;
								num5 = KnownErrorEllipsePA;
								text2 = "Known errors";
								double num24 = Math.Atan2(num18, num19) * (180.0 / Math.PI);
								if (num24 > 180.0)
								{
									num24 = (num24 -= 180.0);
								}
								if (num24 < 0.0)
								{
									num24 += 180.0;
								}
								double num25 = 0.0;
								double num26 = num4 / num3;
								double angle_deg = num5 - num24;
								for (int k = -90; k <= 90; k += 2)
								{
									double x = num3 * Math.Sin((double)k / (180.0 / Math.PI));
									double y = num26 * num3 * Math.Cos((double)k / (180.0 / Math.PI));
									Utilities.RotateXY(x, y, angle_deg, out var _, out var y2);
									if (y2 > num25)
									{
										num25 = y2;
									}
								}
								double num27 = num25 / 8.794143836182533 * AstrometricGeocentricDistance * 6378.137;
								KnownError_as_IncreaseInPathWidth = (diameter_Mean + num27) / diameter_Mean;
								KnownErrorSigma_arcsec = Math.Sqrt(num3 * num3 + num4 * num4);
							}
							else
							{
								if (num12 == 0.0)
								{
									num12 = 0.2;
									text2 = "Star+Assumed";
								}
								else
								{
									text2 = "Star+PeakEphemUncert";
								}
								num4 = (num3 = Math.Sqrt(num12 * num12 + 0.0009));
								num5 = 90.0;
								KnownErrorSigma_arcsec = 0.0;
								KnownError_as_IncreaseInPathWidth = 0.0;
							}
							string Basis;
							bool InvalidDiameter;
							double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(RA4 * (180.0 / Math.PI) / 15.0, num6 * (180.0 / Math.PI), magV, magB, magR, out Basis, out NumMeasures, out InvalidDiameter);
							MinorPlanetPrediction(num14, num22, j, starCatName, RA4, num6, num11, magV, magB, magR, num3, num4, num5, KnownErrorSigma_arcsec, KnownError_as_IncreaseInPathWidth, text2, num17 + ExpandedSearchDistInEarthRadii, starDiaMAS, starReliability, noGaiaPM, gaiaUCAC4PM, duplicateSource, Get_NearbyStars: true, out flag, UseDefaultErrorSettings, AutoSaveSearch, OutputFileName, ShowListAndDisplay: true);
							if (flag)
							{
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
			if (CallingForm != null)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		private static void MinorPlanetPrediction(double EventJD, double H1, int AsteroidFileRecordNumber, string StarCatName, double RAStar_2000, double DecStar_2000, double PiStar, double MagV, double MagB, double MagR, double ErrorEllipseMajor, double ErrorEllipseMinor, double ErrorEllipsePA, double KnownErrorSigma_ArcSec, double KnownError_as_IncreaseInPathWidth, string ErrorBasis, double ExpandedSearchDistance, double StarDiaMAS, out bool CloseEventFound, bool autoOutput, bool AutoSaveSearch, string OutputFileName)
		{
			double starReliability = -1.0;
			int duplicateSource = -1;
			int noGaiaPM = -1;
			int gaiaUCAC4PM = -1;
			bool get_NearbyStars = true;
			MinorPlanetPrediction(EventJD, H1, AsteroidFileRecordNumber, StarCatName, RAStar_2000, DecStar_2000, PiStar, MagV, MagB, MagR, ErrorEllipseMajor, ErrorEllipseMinor, ErrorEllipsePA, KnownErrorSigma_ArcSec, KnownError_as_IncreaseInPathWidth, ErrorBasis, ExpandedSearchDistance, StarDiaMAS, starReliability, noGaiaPM, gaiaUCAC4PM, duplicateSource, get_NearbyStars, out CloseEventFound, autoOutput, AutoSaveSearch, OutputFileName, ShowListAndDisplay: true);
		}

		private static void MinorPlanetPrediction(double EventJD, double H1, int AsteroidFileRecordNumber, string StarCatName, double RAStar_2000, double DecStar_2000, double PiStar, double MagV, double MagB, double MagR, double ErrorEllipseMajor, double ErrorEllipseMinor, double ErrorEllipsePA, double KnownErrorSigma_ArcSec, double KnownError_as_IncreaseInPathWidth, string ErrorBasis, double ExpandedSearchDistance, double StarDiaMAS, double StarReliability, int NoGaiaPM, int GaiaUCAC4PM, int DuplicateSource, bool Get_NearbyStars, out bool CloseEventFound, bool autoOutput, bool AutoSaveSearch, string OutputFileName, bool ShowListAndDisplay)
		{
			MinorPlanetPrediction(EventJD, H1, AsteroidFileRecordNumber, StarCatName, RAStar_2000, DecStar_2000, PiStar, MagV, MagB, MagR, ErrorEllipseMajor, ErrorEllipseMinor, ErrorEllipsePA, -1.0, -1.0, KnownErrorSigma_ArcSec, KnownError_as_IncreaseInPathWidth, ErrorBasis, ExpandedSearchDistance, StarDiaMAS, StarReliability, NoGaiaPM, GaiaUCAC4PM, DuplicateSource, Get_NearbyStars, out CloseEventFound, autoOutput, AutoSaveSearch, OutputFileName, ShowListAndDisplay);
		}

		private static void MinorPlanetPrediction(double EventJD, double H1, int AsteroidFileRecordNumber, string StarCatName, double RAStar_2000, double DecStar_2000, double PiStar, double MagV, double MagB, double MagR, double ErrorEllipseMajor, double ErrorEllipseMinor, double ErrorEllipsePA, double StarErrorRA, double StarErrorDec, double KnownErrorSigma_ArcSec, double KnownError_as_IncreaseInPathWidth, string ErrorBasis, double ExpandedSearchDistance, double StarDiaMAS, double StarReliability, int NoGaiaPM, int GaiaUCAC4PM, int DuplicateSource, bool Get_NearbyStars, out bool CloseEventFound, bool autoOutput, bool AutoSaveSearch, string OutputFileName, bool ShowListAndDisplay)
		{
			double RA_end = 0.0;
			double Dec_end = 0.0;
			double RA_end2 = 0.0;
			double Dec_end2 = 0.0;
			double dRA_ComparedToStar = 0.0;
			double dDec_ComparedToStar = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			bool flag = false;
			bool TwoEvents = false;
			int Year = 2018;
			int Month = 1;
			double day = 1.0;
			double[] MidTpair = new double[2];
			int num3 = 0;
			string text = "";
			int num4 = 0;
			double num5 = 0.0;
			double num6 = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDistance = 0.0;
			bool flag2 = true;
			bool flag3 = false;
			CloseEventFound = false;
			Application.DoEvents();
			double num7 = ElementsAt1Day[AsteroidFileRecordNumber + 1].Meananomaly / (180.0 / Math.PI);
			double num8 = ElementsAt1Day[AsteroidFileRecordNumber + 1].Perihelion / (180.0 / Math.PI);
			double num9 = ElementsAt1Day[AsteroidFileRecordNumber + 1].Node / (180.0 / Math.PI);
			double num10 = ElementsAt1Day[AsteroidFileRecordNumber + 1].I / (180.0 / Math.PI);
			double e = ElementsAt1Day[AsteroidFileRecordNumber + 1].E;
			double a;
			double num11 = (a = ElementsAt1Day[AsteroidFileRecordNumber + 1].A);
			double num12 = num11 * (1.0 - e);
			double num13 = ElementsAt1Day[AsteroidFileRecordNumber].IDNumber;
			string iDName = ElementsAt1Day[AsteroidFileRecordNumber].IDName;
			int epochYear = ElementsAt1Day[AsteroidFileRecordNumber].EpochYear;
			int epochMonth = ElementsAt1Day[AsteroidFileRecordNumber].EpochMonth;
			double epochDay = ElementsAt1Day[AsteroidFileRecordNumber].EpochDay;
			double num14 = Utilities.JD_from_Date(epochYear, epochMonth, epochDay);
			double num15 = ElementsAt1Day[AsteroidFileRecordNumber].Meananomaly / (180.0 / Math.PI);
			double num16 = ElementsAt1Day[AsteroidFileRecordNumber].Perihelion / (180.0 / Math.PI);
			double num17 = ElementsAt1Day[AsteroidFileRecordNumber].Node / (180.0 / Math.PI);
			double num18 = ElementsAt1Day[AsteroidFileRecordNumber].I / (180.0 / Math.PI);
			double e2 = ElementsAt1Day[AsteroidFileRecordNumber].E;
			a = ElementsAt1Day[AsteroidFileRecordNumber].A;
			double num19 = a * (1.0 - e2);
			double h = ElementsAt1Day[AsteroidFileRecordNumber].H0;
			double g_phaseCoeff = ElementsAt1Day[AsteroidFileRecordNumber].G_phaseCoeff;
			double logR_Coeff = ElementsAt1Day[AsteroidFileRecordNumber].LogR_Coeff;
			double num20 = ElementsAt1Day[AsteroidFileRecordNumber].Diameter_Mean;
			double num21 = ElementsAt1Day[AsteroidFileRecordNumber].DiameterUncertainty;
			string Source;
			if (!iDName.Contains("/") && (num20 <= 0.0 || num21 <= 0.0))
			{
				if (!Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo((int)num13, out var Diameter, out var DiameterUncertainty, out Source))
				{
					if (num20 <= 0.0)
					{
						num20 = Math.Pow(10.0, 3.52 - 0.2 * h);
					}
					if (num20 < 0.1)
					{
						num20 = 0.1;
					}
					num21 = num20 / 10.0;
				}
				else
				{
					if (num20 <= 0.0)
					{
						num20 = Diameter;
					}
					if (num21 <= 0.0)
					{
						num21 = DiameterUncertainty;
					}
				}
			}
			int num_Moons = ElementsAt1Day[AsteroidFileRecordNumber].Num_Moons;
			int num_Rings = ElementsAt1Day[AsteroidFileRecordNumber].Num_Rings;
			if (QueryMiriade)
			{
				for (int i = 0; i < BinaryAsteroidsInMiriade.Count; i++)
				{
					int[] array = BinaryAsteroidsInMiriade[i];
					if (num13 == (double)array[0])
					{
						num2 = (double)array[1] / 6378.137;
						break;
					}
				}
			}
			if (e2 > 0.97)
			{
				num19 = Math.Abs(ElementsAt1Day[AsteroidFileRecordNumber].q);
				num12 = num19;
				num15 = 0.0;
				num7 = 0.0;
				flag = true;
			}
			num7 -= num15;
			if (num7 < -Math.PI)
			{
				num7 += Math.PI * 2.0;
			}
			if (num7 > Math.PI)
			{
				num7 -= Math.PI * 2.0;
			}
			num8 -= num16;
			if (num8 < -Math.PI)
			{
				num8 += Math.PI * 2.0;
			}
			if (num8 > Math.PI)
			{
				num8 -= Math.PI * 2.0;
			}
			num9 -= num17;
			if (num9 < -Math.PI)
			{
				num9 += Math.PI * 2.0;
			}
			if (num9 > Math.PI)
			{
				num9 -= Math.PI * 2.0;
			}
			num10 -= num18;
			e -= e2;
			num11 -= a;
			num12 -= num19;
			Utilities.PositionfromElements(EventJD + H1 / 24.0, 0.0, 0.0, 0.0, 0.0, num14, num15, num19, e2, num16, num17, num18, h, g_phaseCoeff + 0.0001, logR_Coeff, 1E-05, out var RA2, out var Dec2, out var RadiusVector, out var AstrometricGeocentricDistance, out var Magnitude, out var Elongation, out var PhaseAngle);
			BessellianElements(RAStar_2000, DecStar_2000, RA2, Dec2, AstrometricGeocentricDistance, out var X, out var Y);
			RA2 += OffsetRA / Math.Cos(Dec2);
			Dec2 += OffsetDec;
			Utilities.PositionfromElements(EventJD + (H1 + 1.0) / 24.0, 0.0, 0.0, 0.0, 0.0, num14, num15, num19, e2, num16, num17, num18, h, g_phaseCoeff + 0.0001, logR_Coeff, 1E-05, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle);
			RA2 += OffsetRA / Math.Cos(Dec2);
			Dec2 += OffsetDec;
			BessellianElements(RAStar_2000, DecStar_2000, RA2, Dec2, AstrometricGeocentricDistance, out var X2, out var Y2);
			double dX = X2 - X;
			double dX2 = Y2 - Y;
			double n = Math.Sqrt(dX * dX + dX2 * dX2);
			double num22 = X * dX + Y * dX2;
			double MinimumDist = (X * dX2 - Y * dX) / n;
			double num23 = H1 - num22 / n / n;
			if (Math.Abs(MinimumDist) > 4.0 + ExpandedSearchDistance + num2 || num23 < -2.0 || num23 > 26.0)
			{
				return;
			}
			double jD_TT = EventJD + num23 / 24.0;
			double num24 = 0.0;
			if (!flag)
			{
				num24 = num23 / 24.0;
			}
			Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out var TrueGeocentricDistance);
			RA2 += OffsetRA / Math.Cos(Dec2);
			Dec2 += OffsetDec;
			BessellianElements(RAStar_2000, DecStar_2000, RA2, Dec2, TrueGeocentricDistance, out X, out Y);
			jD_TT = EventJD + (num23 + 1.0) / 24.0;
			num24 = 0.0;
			if (!flag)
			{
				num24 = (num23 + 1.0) / 24.0;
			}
			Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
			RA2 += OffsetRA / Math.Cos(Dec2);
			Dec2 += OffsetDec;
			BessellianElements(RAStar_2000, DecStar_2000, RA2, Dec2, TrueGeocentricDistance, out X2, out Y2);
			jD_TT = EventJD + (num23 - 1.0) / 24.0;
			num24 = 0.0;
			if (!flag)
			{
				num24 = (num23 - 1.0) / 24.0;
			}
			Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
			RA2 += OffsetRA / Math.Cos(Dec2);
			Dec2 += OffsetDec;
			BessellianElements(RAStar_2000, DecStar_2000, RA2, Dec2, TrueGeocentricDistance, out var X3, out var Y3);
			Utilities.FitQuadraticTo3Points(X3, X, X2, out X, out dX, out var d2X);
			Utilities.FitQuadraticTo3Points(Y3, Y, Y2, out Y, out dX2, out var d2X2);
			GetTforMinimums(num23, X, Y, dX, dX2, d2X, d2X2, ref MidTpair, out TwoEvents, out MinimumDist, out n);
			if (n > 5.0)
			{
				CloseEventFound = true;
			}
			if (Math.Abs(MinimumDist) > 1.01 + num20 / 6378.137 / 2.0 + ExpandedSearchDistance + num2)
			{
				return;
			}
			Utilities.Relativistic_Differential_Correction(jD_TT, RAStar_2000, DecStar_2000, AstrometricGeocentricDistance, out dRA_ComparedToStar, out dDec_ComparedToStar);
			RAStar_2000 -= dRA_ComparedToStar;
			DecStar_2000 -= dDec_ComparedToStar;
			double[] RA3 = new double[4];
			double[] Dec3 = new double[4];
			bool flag4 = false;
			string EphemSource = "";
			for (int j = 0; j <= 1; j++)
			{
				double num25 = MidTpair[j];
				double num26 = 1.0;
				if ((n < 0.5) | UseHorizonsEphemeris)
				{
					num26 = ((n < 0.0625) ? 20.0 : ((n < 0.125) ? 10.0 : ((n < 0.25) ? 5.0 : ((!(n < 0.5)) ? 1.0 : 2.5))));
					if (Utilities.InternetIsAvailable())
					{
						double ErrorMajor = 0.0;
						double ErrorMinor = 0.0;
						double ErrorPA = 0.0;
						double[] Delta;
						if (num13 > 0.0)
						{
							flag4 = http.GetHorizonsAsteroidJ2000Ephemeris(num13.ToString(), EventJD + (num25 - num26 / 1.0) / 24.0, num26, UseTTnotUT: true, UseOldHorizonsOrbit, OldHorizonsOrbitForQueryString, out RA3, out Dec3, out Delta, out EphemSource, out ErrorMajor, out ErrorMinor, out ErrorPA);
						}
						else if (iDName.Contains("("))
						{
							int num27 = iDName.IndexOf("(");
							int num28 = iDName.IndexOf(")");
							flag4 = ((num28 <= num27) ? http.GetHorizonsAsteroidJ2000Ephemeris(iDName, EventJD + (num25 - num26 / 1.0) / 24.0, num26, UseTTnotUT: true, UseOldHorizonsOrbit, OldHorizonsOrbitForQueryString, out RA3, out Dec3, out Delta, out EphemSource, out ErrorMajor, out ErrorMinor, out ErrorPA) : http.GetHorizonsCometApparentEphemeris_J2000_or_Apparent(iDName.Substring(num27 + 1, num28 - num27 - 1), EventJD + (num25 - num26 / 1.0) / 24.0, num26, UseTTnotUT: true, IsJ2000NotApparent: true, out RA3, out Dec3, out Delta, out EphemSource, out ErrorMajor, out ErrorMinor, out ErrorPA, out var _, out var _, out var _));
						}
						if (StarErrorRA >= 0.0)
						{
							double num29 = ErrorPA / (180.0 / Math.PI);
							double num30 = ErrorMajor * Math.Sin(num29);
							double num31 = ErrorMajor * Math.Cos(num29);
							double num32 = ErrorMinor * Math.Sin(num29 - Math.PI / 2.0);
							double num33 = ErrorMinor * Math.Cos(num29 - Math.PI / 2.0);
							double num34 = Math.Sqrt(num30 * num30 + StarErrorRA * StarErrorRA);
							if (num30 < 0.0)
							{
								num34 = 0.0 - num34;
							}
							double num35 = Math.Sqrt(num31 * num31 + StarErrorDec * StarErrorDec);
							if (num31 < 0.0)
							{
								num35 = 0.0 - num35;
							}
							double num36 = Math.Sqrt(num32 * num32 + StarErrorRA * StarErrorRA);
							if (num32 < 0.0)
							{
								num36 = 0.0 - num36;
							}
							double num37 = Math.Sqrt(num33 * num33 + StarErrorDec * StarErrorDec);
							if (num33 < 0.0)
							{
								num37 = 0.0 - num37;
							}
							double num38 = Math.Sqrt(num34 * num34 + num35 * num35);
							double num39 = Math.Sqrt(num36 * num36 + num37 * num37);
							double num40 = Math.Atan2(num34, num35) * (180.0 / Math.PI);
							ErrorEllipseMajor = num38;
							ErrorEllipseMinor = num39;
							ErrorEllipsePA = num40;
						}
					}
					else
					{
						flag4 = false;
					}
				}
				CloseEventFound = true;
				jD_TT = EventJD + (num25 - num26 / 1.0) / 24.0;
				num24 = 0.0;
				if (!flag)
				{
					num24 = (num25 - num26) / 24.0;
				}
				Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
				if (flag4)
				{
					RA2 = RA3[0];
					Dec2 = Dec3[0];
				}
				RA2 += OffsetRA / Math.Cos(Dec2);
				Dec2 += OffsetDec;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end, out Dec_end);
				Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				BessellianElements(RA_end, Dec_end, RA_end2, Dec_end2, TrueGeocentricDistance, out X3, out Y3);
				jD_TT = EventJD + (num25 + 2.0 * num26 / 1.0) / 24.0;
				num24 = 0.0;
				if (!flag)
				{
					num24 = (num25 + 2.0 * num26) / 24.0;
				}
				Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
				if (flag4)
				{
					RA2 = RA3[3];
					Dec2 = Dec3[3];
				}
				RA2 += OffsetRA / Math.Cos(Dec2);
				Dec2 += OffsetDec;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end, out Dec_end);
				Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				BessellianElements(RA_end, Dec_end, RA_end2, Dec_end2, TrueGeocentricDistance, out var X4, out var Y4);
				jD_TT = EventJD + (num25 + num26 / 1.0) / 24.0;
				num24 = 0.0;
				if (!flag)
				{
					num24 = (num25 + num26) / 24.0;
				}
				Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
				if (flag4)
				{
					RA2 = RA3[2];
					Dec2 = Dec3[2];
				}
				RA2 += OffsetRA / Math.Cos(Dec2);
				Dec2 += OffsetDec;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end, out Dec_end);
				Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				BessellianElements(RA_end, Dec_end, RA_end2, Dec_end2, TrueGeocentricDistance, out X2, out Y2);
				double num41 = RA_end2;
				double num42 = Dec_end2;
				jD_TT = EventJD + num25 / 24.0;
				num24 = 0.0;
				if (!flag)
				{
					num24 = num25 / 24.0;
				}
				Utilities.PositionfromElements(jD_TT, 0.0, 0.0, 0.0, 0.0, num14 + num24, num15 + num7 * num24, num19 + num12 * num24, e2 + e * num24, num16 + num8 * num24, num17 + num9 * num24, num18 + num10 * num24, h, g_phaseCoeff, logR_Coeff, 0.0, out RA2, out Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out TrueGeocentricDistance);
				if (flag4)
				{
					RA2 = RA3[1];
					Dec2 = Dec3[1];
				}
				RA2 += OffsetRA / Math.Cos(Dec2);
				Dec2 += OffsetDec;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end, out Dec_end);
				Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, jD_TT, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				BessellianElements(RA_end, Dec_end, RA_end2, Dec_end2, TrueGeocentricDistance, out X, out Y);
				double num43 = num41 - RA_end2;
				if (Math.Abs(num43) > Math.PI)
				{
					num43 -= Math.PI * 2.0 * (double)Math.Sign(num43);
				}
				num43 = num43 * 3600.0 * 1.0;
				double num44 = (num42 - Dec_end2) * 3600.0 * 1.0;
				List<BinaryAsteroidOffsets> BinaryOffsets = new List<BinaryAsteroidOffsets>();
				BinaryAsteroidOffsets binaryAsteroidOffsets = new BinaryAsteroidOffsets();
				binaryAsteroidOffsets.Offset_RA_mas[0] = (binaryAsteroidOffsets.Offset_RA_mas[1] = (binaryAsteroidOffsets.Offset_RA_mas[2] = (binaryAsteroidOffsets.Offset_RA_mas[3] = 0.0)));
				binaryAsteroidOffsets.Offset_Dec_mas[0] = (binaryAsteroidOffsets.Offset_Dec_mas[1] = (binaryAsteroidOffsets.Offset_Dec_mas[2] = (binaryAsteroidOffsets.Offset_Dec_mas[3] = 0.0)));
				binaryAsteroidOffsets.ComponentName = iDName;
				binaryAsteroidOffsets.ComponentSeqNum = -1;
				binaryAsteroidOffsets.ComponentDiameter = num20;
				binaryAsteroidOffsets.ComponentDiameterUncertainty = num21;
				binaryAsteroidOffsets.SolutionType = "Barycenter";
				binaryAsteroidOffsets.SolutionID = "0";
				BinaryOffsets.Add(binaryAsteroidOffsets);
				num = 8794.143836182533 / AstrometricGeocentricDistance;
				MiriadeWasQueried = null;
				if (QueryMiriade)
				{
					for (int k = 0; k < BinaryAsteroidsInMiriade.Count; k++)
					{
						int[] array2 = BinaryAsteroidsInMiriade[k];
						if (num13 == (double)array2[0])
						{
							MiriadeWasQueried = XMLprocess.GetMiraideEphemeris((int)num13, EventJD + num25 / 24.0, ref BinaryOffsets, SaveMiriadeResponse);
							break;
						}
					}
				}
				NumberOfPredictionsForBinarySystem = BinaryOffsets.Count;
				for (int l = 0; l < BinaryOffsets.Count; l++)
				{
					flag2 = true;
					double num45 = 0.0;
					double num46 = 0.0;
					double num47 = 0.0;
					if (MiriadeFirstOnly)
					{
						int.TryParse(BinaryOffsets[l].SolutionID, out var result);
						if (result > 1)
						{
							break;
						}
					}
					Utilities.FitCubicTo4_EqualSpaced_Points(X3 + BinaryOffsets[l].Offset_RA_mas[0] / num, X + BinaryOffsets[l].Offset_RA_mas[1] / num, X2 + BinaryOffsets[l].Offset_RA_mas[2] / num, X4 + BinaryOffsets[l].Offset_RA_mas[3] / num, out var Xat, out dX, out d2X, out var d3X);
					Utilities.FitCubicTo4_EqualSpaced_Points(Y3 + BinaryOffsets[l].Offset_Dec_mas[0] / num, Y + BinaryOffsets[l].Offset_Dec_mas[1] / num, Y2 + BinaryOffsets[l].Offset_Dec_mas[2] / num, Y4 + BinaryOffsets[l].Offset_Dec_mas[3] / num, out var Xat2, out dX2, out d2X2, out var d3X2);
					if (num26 > 1.0)
					{
						dX /= num26;
						d2X /= num26 * num26;
						d3X /= num26 * num26 * num26;
						dX2 /= num26;
						d2X2 /= num26 * num26;
						d3X2 /= num26 * num26 * num26;
					}
					double num48 = dX;
					double num49 = dX2;
					n = Math.Sqrt(num48 * num48 + num49 * num49);
					double num50 = Xat;
					double num51 = Xat2;
					double num52 = EventJD;
					double num53;
					for (num53 = num25 - Utilities.delta_T(num52) / 3600.0; num53 >= 24.0; num53 -= 24.0)
					{
						num52 += 1.0;
					}
					for (; num53 < 0.0; num53 += 24.0)
					{
						num52 -= 1.0;
					}
					text = Utilities.Date_from_JD(num52, 0);
					num4 = (int)Math.Floor(num53);
					Utilities.Date_from_JD(num52, out Year, out Month, out day);
					double num54 = (Utilities.SiderealTime_deg(num52, Apparent: true) + 360.98568 * num53 / 24.0) % 360.0;
					for (num5 = 180.0 / Math.PI * RAStar_2000 - num54; num5 > 180.0; num5 -= 360.0)
					{
					}
					for (; num5 < -180.0; num5 += 360.0)
					{
					}
					for (num6 = 180.0 / Math.PI * RA_end - num54; num6 > 180.0; num6 -= 360.0)
					{
					}
					for (; num6 < -180.0; num6 += 360.0)
					{
					}
					Utilities.QuickPlanet(num52 + num53 / 24.0, 3, EquinoxOfDate: true, out RA, out Dec, out GeocentricDistance);
					double num55;
					for (num55 = RA * (180.0 / Math.PI) - num54; num55 > 180.0; num55 -= 360.0)
					{
					}
					for (; num55 < -180.0; num55 += 360.0)
					{
					}
					double num56 = Dec * (180.0 / Math.PI);
					double componentDiameter = BinaryOffsets[l].ComponentDiameter;
					num21 = BinaryOffsets[l].ComponentDiameterUncertainty;
					if (ApplySiteRestriction)
					{
						double num57 = 0.0;
						double num58 = 0.0;
						double num59 = 0.0;
						double num60 = Dec_end * (180.0 / Math.PI);
						float z = 0f;
						for (int m = 0; m <= 5; m++)
						{
							Maps.SetGlobeOrientation((num6 - num57 * 15.0) / (180.0 / Math.PI), num60 / (180.0 / Math.PI));
							Maps.GlobeCoords(SiteLongitudeTest / (180.0 / Math.PI), SiteLatitudeTest / (180.0 / Math.PI), out var x, out var y, out z, Mirrored: false);
							num58 = num50 - (double)x + dX * num57 + d2X * num57 * num57 + d3X * num57 * num57 * num57;
							num59 = num51 - (double)y + dX2 * num57 + d2X2 * num57 * num57 + d3X2 * num57 * num57 * num57;
							double num61 = (0.0 - (num58 * dX + num59 * dX2)) / n / n;
							num57 += num61;
							if (Math.Abs(num61) < 0.003)
							{
								break;
							}
						}
						double num62 = Math.Abs(num58 * dX2 - num59 * dX) / n;
						double num63 = Math.Asin(z) * (180.0 / Math.PI);
						Maps.SetGlobeOrientation((num55 - num57 * 15.0) / (180.0 / Math.PI), Dec);
						Maps.GlobeCoords(SiteLongitudeTest / (180.0 / Math.PI), SiteLatitudeTest / (180.0 / Math.PI), out var _, out var _, out var z2, Mirrored: false);
						double num64 = Math.Asin(z2) * (180.0 / Math.PI);
						if (num62 > SearchDistance + componentDiameter / 2.0 / 6378.137)
						{
							flag2 = false;
						}
						if (num63 < -5.0)
						{
							flag2 = false;
						}
						if (num64 > 0.0 && MagV > 5.0)
						{
							flag2 = false;
						}
						if (num64 > -8.0 && MagV > 8.0)
						{
							flag2 = false;
						}
						if (n < 1.0)
						{
							flag2 = true;
						}
						if (!flag2)
						{
							continue;
						}
					}
					double num65 = componentDiameter + StarDiaMAS / 3600000.0 / (180.0 / Math.PI) * AstrometricGeocentricDistance * 149600000.0;
					double num66 = num65 / componentDiameter;
					num45 = KnownErrorSigma_ArcSec;
					double A = ErrorEllipseMajor;
					double B = ErrorEllipseMinor;
					double num67 = ErrorEllipsePA;
					if (l > 0)
					{
						double num68 = ErrorEllipseMajor * Math.Sin(ErrorEllipsePA / (180.0 / Math.PI));
						double num69 = Math.Sqrt(num68 * num68 + BinaryOffsets[l].UncertaintyInX_asec * BinaryOffsets[l].UncertaintyInX_asec);
						double num70 = ErrorEllipseMajor * Math.Cos(ErrorEllipsePA / (180.0 / Math.PI));
						num70 = Math.Sqrt(num70 * num70 + BinaryOffsets[l].UncertaintyInY_asec * BinaryOffsets[l].UncertaintyInY_asec);
						A = Math.Sqrt(num69 * num69 + num70 * num70);
						double num71 = ErrorEllipseMinor * Math.Sin((ErrorEllipsePA + 90.0) / (180.0 / Math.PI));
						double num72 = Math.Sqrt(num71 * num71 + BinaryOffsets[l].UncertaintyInX_asec * BinaryOffsets[l].UncertaintyInX_asec);
						double num73 = ErrorEllipseMinor * Math.Cos((ErrorEllipsePA + 90.0) / (180.0 / Math.PI));
						num73 = Math.Sqrt(num73 * num73 + BinaryOffsets[l].UncertaintyInY_asec * BinaryOffsets[l].UncertaintyInY_asec);
						B = Math.Sqrt(num72 * num72 + num73 * num73);
						num67 = Math.Atan(num69 / num70) * (180.0 / Math.PI);
						if (A < B)
						{
							Utilities.Swap(ref A, ref B);
							num67 = ((!(num67 > 90.0)) ? (num67 + 90.0) : (num67 - 90.0));
						}
						if (ErrorEllipsePA > 90.0)
						{
							num67 = 180.0 - num67;
						}
					}
					double error_AsIncreaseInPathWidths;
					if (KnownErrorSigma_ArcSec > 0.0)
					{
						double num74 = Math.Atan2(dX, dX2) * (180.0 / Math.PI);
						if (num74 > 180.0)
						{
							num74 = (num74 -= 180.0);
						}
						if (num74 < 0.0)
						{
							num74 += 180.0;
						}
						double num75 = 0.0;
						double num76 = B / A;
						double angle_deg = num67 - num74;
						for (int num77 = -90; num77 <= 90; num77 += 2)
						{
							double x3 = A * Math.Sin((double)num77 / (180.0 / Math.PI));
							double y3 = num76 * A * Math.Cos((double)num77 / (180.0 / Math.PI));
							Utilities.RotateXY(x3, y3, angle_deg, out var _, out var y4);
							if (Math.Abs(y4) > num75)
							{
								num75 = Math.Abs(y4);
							}
						}
						double num78 = num75 / 8.794143836182533 * TrueGeocentricDistance * 6378.137;
						error_AsIncreaseInPathWidths = 1.0 + num78 / num65;
					}
					else if (num46 > 0.0 && num47 > 0.0)
					{
						double num79 = Math.Atan2(dX, dX2);
						double num80 = Math.Sqrt(num47 / 1000.0 * Math.Sin(num79) * num47 / 1000.0 * Math.Sin(num79) + num46 / 1000.0 * Math.Cos(num79) * num46 / 1000.0 * Math.Cos(num79)) / (componentDiameter / 6378.137 * 8.794143836182533 / AstrometricGeocentricDistance);
						error_AsIncreaseInPathWidths = 1.0 + num80 * componentDiameter / num65;
					}
					else
					{
						double num81 = ((!(componentDiameter > 0.0)) ? (1.0 + A / (0.0006893975338082682 / AstrometricGeocentricDistance)) : (1.0 + A / (componentDiameter / 6378.137 * 8.794143836182533 / AstrometricGeocentricDistance)));
						error_AsIncreaseInPathWidths = (num81 + (num66 - 1.0)) / num66;
					}
					double maxDurn = componentDiameter * num66 / n * 3600.0 / 6378.137;
					num3 = 0;
					if (IncludeWDSSearchInSearch)
					{
						if (SearchWDS | SearchIF)
						{
							num3 = Interferometric_Plus_WDS.StarIsInDoubleCats(RAStar_2000 * (180.0 / Math.PI) / 15.0, DecStar_2000 * (180.0 / Math.PI));
						}
						if (Interferometric_Plus_WDS.IsInAAVSO(RAStar_2000 * (180.0 / Math.PI) / 15.0, DecStar_2000 * (180.0 / Math.PI)).Length > 1)
						{
							num3 += 4;
						}
					}
					if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
					{
						Kepler2.Initialise_Kepler2_ForAsteroids();
					}
					flag3 = Kepler2.StarInKepler2(RAStar_2000 * (180.0 / Math.PI), DecStar_2000 * (180.0 / Math.PI));
					OccultationElements occultationElements = new OccultationElements();
					occultationElements.IsAsteroid = true;
					bool isPlanet = (occultationElements.IsPlanetaryMoon = false);
					occultationElements.IsPlanet = isPlanet;
					occultationElements.ObjectNumber = num13.ToString();
					occultationElements.AsteroidNumber = (int)num13;
					occultationElements.ObjectName = BinaryOffsets[l].ComponentName;
					occultationElements.EventYear = Year;
					occultationElements.EventMonth = Month;
					occultationElements.EventDay = (int)day;
					occultationElements.EventJD = num52;
					occultationElements.MidTime_Hrs = num53;
					occultationElements.XatMin = Xat;
					occultationElements.dX = dX;
					occultationElements.d2X = d2X;
					occultationElements.d3X = d3X;
					occultationElements.YatMin = Xat2;
					occultationElements.dY = dX2;
					occultationElements.d2Y = d2X2;
					occultationElements.d3Y = d3X2;
					occultationElements.StarCatName = StarCatName;
					occultationElements.RA_Star2000 = RAStar_2000;
					occultationElements.Dec_Star2000 = DecStar_2000;
					occultationElements.RA_Star_Apparent = RA_end;
					occultationElements.Dec_Star_Apparent = Dec_end;
					occultationElements.UsesApparentStar = true;
					if ((MagB == 0.0 || MagB > 20.0) && MagR > 0.0 && MagR < 20.0)
					{
						MagB = (1.54 * MagV - MagR) / 0.54;
					}
					else if (MagB > 0.0 && MagB < 20.0 && (MagR == 0.0 || MagR > 20.0))
					{
						MagR = 1.54 * MagV - 0.54 * MagB;
					}
					occultationElements.mB = MagB;
					occultationElements.mV = MagV;
					occultationElements.mR = MagR;
					occultationElements.StarDiamMAS = StarDiaMAS;
					occultationElements.DoubleStarCode = num3;
					occultationElements.IsKepler2Star = flag3;
					double Elongation2 = (occultationElements.MagAsteroid = (occultationElements.MagVAsteroid = Magnitude));
					occultationElements.MagRAsteroid = Magnitude - 0.45;
					if (MagValues.AsteroidMagnitudeFileExists)
					{
						if (occultationElements.AsteroidNumber <= 0)
						{
							Elongation2 = (occultationElements.MagVAsteroid = (occultationElements.MagRAsteroid = -5.0));
						}
						else
						{
							Utilities.QuickSolarElongation_PhaseAngle(EventJD, RAStar_2000, DecStar_2000, AstrometricGeocentricDistance, out Elongation2, out var PhaseAngle2, out var SolarDistObject);
							int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord(occultationElements.AsteroidNumber);
							if (colorMagnitudeRecord < 0)
							{
								Elongation2 = (occultationElements.MagVAsteroid = (occultationElements.MagRAsteroid = -5.0));
							}
							else
							{
								MagValues.ColorMagnitudeData[colorMagnitudeRecord].GetColorMagnitudes(RAStar_2000, DecStar_2000, RadiusVector, AstrometricGeocentricDistance, PhaseAngle2 / (180.0 / Math.PI), out var MagV2, out var MagR2, out SolarDistObject, out Elongation2);
								occultationElements.MagVAsteroid = MagV2;
								occultationElements.MagRAsteroid = MagR2;
							}
						}
					}
					occultationElements.AsteroidDiameter = componentDiameter;
					occultationElements.AsteroidDiameterUncertainty = num21;
					occultationElements.AsteroidShadowDiameter_Penumbral = num65;
					occultationElements.AsteroidShadowDiameter_Umbral = componentDiameter - StarDiaMAS / 3600000.0 / (180.0 / Math.PI) * AstrometricGeocentricDistance * 149600000.0;
					occultationElements.AsteroidDiameterArcSec = componentDiameter / 725.3 / AstrometricGeocentricDistance;
					double num88 = 0.0;
					double num89 = StarDiaMAS * StarDiaMAS - Math.Pow(componentDiameter / 0.7253 / AstrometricGeocentricDistance, 2.0);
					if (num89 > 0.0)
					{
						num88 = num89 / StarDiaMAS / StarDiaMAS;
					}
					double num90 = -2.5 * Math.Log10(num88 + 1E-09);
					double ExtraMagG = 30.0;
					double ExtraMagR = 30.0;
					int AllStars = -1;
					int BrightStars = -1;
					if (Get_NearbyStars)
					{
						Gaia.GetNearbyStarsFromCoords(StarCatName, RAStar_2000, DecStar_2000, MagV, Utilities.BesselianYear(num52) - 2000.0, 15.0, out ExtraMagG, out ExtraMagR, out BrightStars, out AllStars, out Source);
					}
					occultationElements.NearbyStars_Bright = BrightStars;
					occultationElements.NearbyStars_All = AllStars;
					occultationElements.NearbyStars_MagAdjusted = ExtraMagG < 20.0 || ExtraMagR < 20.0;
					double mag;
					if (occultationElements.MagVAsteroid > 0.0)
					{
						Elongation2 = (occultationElements.CombinedMagnitudeV = Utilities.CombinedMagnitude(ExtraMagG, occultationElements.MagVAsteroid));
						mag = Elongation2;
					}
					else
					{
						mag = Utilities.CombinedMagnitude(ExtraMagG, occultationElements.MagAsteroid);
					}
					double num92 = Utilities.CombinedMagnitude(mag, MagV);
					double num93 = Utilities.CombinedMagnitude(mag, MagV + num90);
					occultationElements.MagDropV = num93 - num92;
					if (MagR != 0.0 && MagR < 30.0)
					{
						double mag2;
						if (occultationElements.MagRAsteroid > 0.0)
						{
							Elongation2 = (occultationElements.CombinedMagnitudeR = Utilities.CombinedMagnitude(ExtraMagR, occultationElements.MagRAsteroid));
							mag2 = Elongation2;
						}
						else
						{
							mag2 = Utilities.CombinedMagnitude(ExtraMagR, occultationElements.MagAsteroid - 0.45);
						}
						double num95 = Utilities.CombinedMagnitude(mag2, MagR);
						double num96 = Utilities.CombinedMagnitude(mag2, MagR + num90);
						occultationElements.MagDropR = num96 - num95;
					}
					else
					{
						occultationElements.MagDropR = -5.0;
					}
					occultationElements.MaxDurn = maxDurn;
					occultationElements.DistAsteroid = AstrometricGeocentricDistance;
					occultationElements.hourly_dRA = num43 * (180.0 / Math.PI) / 15.0;
					occultationElements.hourly_dDec = num44 * (180.0 / Math.PI);
					occultationElements.NumAsteroidRings = num_Rings;
					occultationElements.NumAsteroidMoons = num_Moons;
					if (l > 0)
					{
						if (!BinaryOffsets[l].ComponentName.Contains(BinaryOffsets[0].ComponentName.Trim()))
						{
							int num99 = (occultationElements.NumAsteroidRings = (occultationElements.NumAsteroidMoons = 0));
						}
						occultationElements.ObjectName = occultationElements.ObjectName + " #" + BinaryOffsets[l].SolutionID;
					}
					occultationElements.HasShapeModel = DisplayMPOccultations.IsInDAMIT((int)num13) | DisplayMPOccultations.IsInISAM((int)num13);
					occultationElements.AsteroidClass = ElementsAt1Day[AsteroidFileRecordNumber + 1].AsteroidClass;
					occultationElements.SubstellarLongitude_J2000_deg = num5;
					occultationElements.SubstellarLatitude_J2000_deg = DecStar_2000 * (180.0 / Math.PI);
					occultationElements.SubstellarLongitude_OfDate_deg = num6;
					occultationElements.SubstellarLatitude_OfDate_deg = Dec_end * (180.0 / Math.PI);
					occultationElements.SubSolarLongitude_OfDate_deg = num55;
					occultationElements.SubSolarLatitude_OfDate_deg = num56;
					occultationElements.SolarElongation = Math.Acos(Math.Sin(num56 / (180.0 / Math.PI)) * Math.Sin(DecStar_2000) + Math.Cos(num56 / (180.0 / Math.PI)) * Math.Cos(DecStar_2000) * Math.Cos((num5 - num55) / (180.0 / Math.PI))) * (180.0 / Math.PI);
					occultationElements.ForJWST = false;
					occultationElements.EpochYear = epochYear;
					occultationElements.EpochMonth = epochMonth;
					occultationElements.EpochDay = epochDay;
					occultationElements.MeanAnomaly = num15;
					occultationElements.Perihelion = num16;
					occultationElements.Node = num17;
					occultationElements.i = num18;
					occultationElements.e = e2;
					occultationElements.a = a;
					occultationElements.q = num19;
					occultationElements.MagConst_H0 = h;
					occultationElements.MagCoeff_logR = logR_Coeff;
					occultationElements.MagSlopeConst_G = g_phaseCoeff;
					occultationElements.delta_MeanAnomaly = num7;
					occultationElements.delta_Perihelion = num8;
					occultationElements.delta_Node = num9;
					occultationElements.delta_i = num10;
					occultationElements.delta_e = e;
					occultationElements.delta_a = num11;
					occultationElements.delta_q = num12;
					occultationElements.OrbitSource = ElementsAt1Day[AsteroidFileRecordNumber + 1].OrbitSource.Trim().Replace(".NI", "(not integrated)") + ElementsAt1Day[AsteroidFileRecordNumber + 1].OrbitDate.Trim();
					if (l > 0)
					{
						OccultationElements occultationElements2 = occultationElements;
						occultationElements2.OrbitSource = occultationElements2.OrbitSource + " Binary solution " + BinaryOffsets[l].SolutionID + " : " + BinaryOffsets[l].SolutionType;
					}
					if (flag4)
					{
						occultationElements.OrbitSource = EphemSource + "+Ephem";
						ErrorBasis = "Known errors";
					}
					occultationElements.Error_AsIncreaseInPathWidths = error_AsIncreaseInPathWidths;
					occultationElements.ErrorEllipseMajorAxis = A;
					occultationElements.ErrorEllipseMinorAxis = B;
					occultationElements.ErrorEllipsePA = num67;
					if (KnownErrorSigma_ArcSec > 0.0)
					{
						occultationElements.KnownSigma_StarAsterPosns = num45;
					}
					else
					{
						occultationElements.KnownSigma_StarAsterPosns = A;
					}
					occultationElements.ErrorBasis = ErrorBasis;
					if (l > 0)
					{
						occultationElements.ErrorBasis += " + binary orbit";
					}
					occultationElements.StarReliability = StarReliability;
					occultationElements.NoGaiaPM = NoGaiaPM;
					occultationElements.GaiaUCAC4PM = GaiaUCAC4PM;
					occultationElements.DuplicateSource = DuplicateSource;
					Utilities.QuickMoon(EventJD + num53 / 24.0, out var RA4, out var Dec4, out var _, out var _, out var _);
					Utilities.Distance(RA4, Dec4, RA, Dec, out var Distance, out Elongation2);
					Utilities.Distance(RA4, Dec4, RAStar_2000, DecStar_2000, out var Distance2, out Elongation2);
					occultationElements.StarMoonElongation_deg = Distance2 * (180.0 / Math.PI);
					occultationElements.MoonPhase_percent = 50.0 * (1.0 - Math.Cos(Distance));
					occultationElements.PredictionMJD = Utilities.MJD_now();
					string text2 = StarCatName.Trim().PadLeft(6).Replace(' ', '_');
					occultationElements.EventID = string.Format("{0,-15}", Year + Month.ToString().PadLeft(2, '0') + day.ToString().PadLeft(2, '0') + "_" + text2.Substring(text2.Length - 6, 6));
					if (flag4)
					{
						occultationElements.EventID += "_H";
					}
					if (l > 0)
					{
						occultationElements.EventID = occultationElements.EventID + "_M" + (char)(97 + BinaryOffsets[l].ComponentSeqNum) + BinaryOffsets[l].SolutionID;
					}
					List<int> list = new List<int>();
					if (BinaryAsteroids.BinElements.Count < 1)
					{
						BinaryAsteroids.Fill_AllAsteroids();
					}
					BinaryAsteroids.GetAsteroidRecord_fromNumberName((int)num13, iDName, list);
					int count = list.Count;
					List<BinaryAsteroidElements> list2 = new List<BinaryAsteroidElements>();
					for (int num100 = 0; num100 < count; num100++)
					{
						BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
						binaryAsteroidElements = BinaryAsteroids.BinElements[list[num100]];
						list2.Add(binaryAsteroidElements);
					}
					occultationElements.AsteroidMoons = list2;
					if (AutoSaveSearch)
					{
						using StreamWriter streamWriter = new StreamWriter(OutputFileName, append: true);
						streamWriter.Write(occultationElements.XML_Elements());
					}
					if (ShowListAndDisplay)
					{
						DisplayMPOccultations.OccElements.Add(occultationElements);
					}
				}
				if (!TwoEvents)
				{
					break;
				}
			}
			if (CallingForm != null && flag2)
			{
				string text3 = " ";
				if (num3 > 0)
				{
					text3 = "#";
				}
				CallingForm.lstResults.get_Items().Add((object)(StarCatName.PadRight(20) + text3 + " on " + text + "  at " + num4.ToString().PadLeft(2) + "hrs"));
				((Control)CallingForm.lstResults).Refresh();
			}
		}

		internal static void GetTforMinimums(double H2, double X0, double Y0, double dX, double dY, double d2X, double d2Y, ref double[] MidTpair, out bool TwoEvents, out double MinimumDist, out double n)
		{
			double num = 0.0;
			bool flag = false;
			int num2 = 0;
			TwoEvents = false;
			n = Math.Sqrt(dX * dX + dY * dY);
			double num3 = X0 * dX + Y0 * dY;
			MinimumDist = (X0 * dY - Y0 * dX) / n;
			MidTpair[0] = H2 - num3 / n / n;
			if (((Math.Abs(dX) > 0.5) | (Math.Abs(dY) > 0.5)) || ((Math.Abs(dX / d2X) > 200.0) | (Math.Abs(dY / d2Y) > 200.0)))
			{
				return;
			}
			for (double num4 = -10.0; num4 < 10.0; num4 += 0.1)
			{
				double num5 = X0 + dX * num4 + d2X * num4 * num4;
				double num6 = Y0 + dY * num4 + d2Y * num4 * num4;
				double num7 = Math.Sqrt(num5 * num5 + num6 * num6);
				if (num4 > -50.0 && !(num7 < num) && flag)
				{
					MidTpair[num2] = H2 + num4;
					if (num2 == 0)
					{
						MinimumDist = num7;
					}
					else if (num7 < MinimumDist)
					{
						MinimumDist = num7;
					}
					num2++;
				}
				if (num2 <= 1)
				{
					_ = 15.0;
					flag = num7 < num;
					num = num7;
					continue;
				}
				break;
			}
		}

		internal static void SortOccelmntFileByDate(string OutputFileName)
		{
			List<SortByT> list = new List<SortByT>();
			char[] array = new char[540];
			string path = Path.GetDirectoryName(OutputFileName) + "\\" + Path.GetFileNameWithoutExtension(OutputFileName) + "_by_T" + Path.GetExtension(OutputFileName);
			using (StreamReader streamReader = new StreamReader(OutputFileName))
			{
				long num = streamReader.BaseStream.Length / 540;
				for (int i = 0; i < num; i++)
				{
					streamReader.Read(array, 0, 540);
					SortByT sortByT = new SortByT();
					sortByT.Record = i;
					sortByT.EventDate = string.Concat(str1: ("JanFebMarAprMayJunJulAugSepOctNovDec".IndexOf(new string(array, 6, 3)) / 3).ToString().PadLeft(2, '0'), str0: new string(array, 1, 4), str2: new string(array, 10, 2).Replace(" ", "0"), str3: new string(array, 38, 9).Replace(" ", "0"));
					list.Add(sortByT);
				}
			}
			list.Sort();
			using StreamWriter streamWriter = new StreamWriter(path);
			FileStream fileStream = new FileStream(OutputFileName, FileMode.Open, FileAccess.Read);
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			for (int j = 0; j < list.Count; j++)
			{
				fileStream.Seek(list[j].Record * 540, SeekOrigin.Begin);
				streamWriter.Write(new string(binaryReader.ReadChars(540)));
			}
		}

		internal static bool CheckElementFileLength_Abort(string ElementFile)
		{
			//IL_0066: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Invalid comparison between Unknown and I4
			bool result = false;
			if (new FileInfo(ElementFile).Length % 145 != 0L && (int)MessageBox.Show("The file length of the file of asteroid elements -\r\n" + ElementFile + "\r\nis of incorrect length. \r\n\r\nThe length should be a multiple of " + string.Format("{0,1:f0}", 145) + " bytes. \r\n\r\nIf you continue with the search, an error might occur.\r\n\r\nDo you want to continue with the search? ", "Incorrect File length", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				result = true;
			}
			return result;
		}
	}
}
