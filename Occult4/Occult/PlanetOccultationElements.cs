using System;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	internal class PlanetOccultationElements
	{
		public static string AppPath;

		public static AsteroidSearch CallingForm;

		public static bool AbortFlag = false;

		internal static double RACorrection = 0.0;

		internal static double DecCorrection = 0.0;

		internal static bool SearchWDS = false;

		internal static bool SearchIF = false;

		internal static bool IncludeWDSSearchInSearch = true;

		public static bool LimitSearchToKepler2 = false;

		internal static double MagDropLimitForSatelliteSearches = -1.0;

		internal static bool ApplySiteRestriction = false;

		internal static double SiteLongitudeTest = 0.0;

		internal static double SiteLatitudeTest = 0.0;

		internal static double SearchDistance = 0.2;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double ParallaxAtUnitDistance = 4.26352124542639E-05;

		internal static bool HorizonsIsDown = false;

		private static double[] EventTimes = new double[24];

		private static int[] EventTypes = new int[24];

		internal const double StandardExposureDurn = 0.0367;

		internal static double SingleExposureMultiplierToGetToMag;

		internal static double NumberOfExposuresRequired_ToBeSure;

		private static void BessellianElements(double RAStar, double DecStar, double RAAsteroid, double DecAsteroid, double DistanceAsteroid, out double X, out double Y)
		{
			double num = 4.26345E-05 / DistanceAsteroid;
			X = Math.Cos(DecAsteroid) * Math.Sin(RAAsteroid - RAStar) / num;
			Y = (Math.Sin(DecAsteroid) * Math.Cos(DecStar) - Math.Cos(DecAsteroid) * Math.Sin(DecStar) * Math.Cos(RAAsteroid - RAStar)) / num;
		}

		internal static void PlanetParameters(int Planet, out string PlanetName, out int NumMoons, out int NumLargeMoons, out double PlanetRadius, out double PlanetDiameterUncertainty_km)
		{
			PlanetName = "";
			NumMoons = (NumLargeMoons = 0);
			PlanetRadius = 1.0;
			PlanetDiameterUncertainty_km = 0.1;
			switch (Planet)
			{
			case 1:
				PlanetName = "Mercury";
				NumMoons = 0;
				NumLargeMoons = 0;
				PlanetRadius = 0.3825096;
				PlanetDiameterUncertainty_km = 2.0;
				break;
			case 2:
				PlanetName = "Venus  ";
				NumMoons = 0;
				NumLargeMoons = 0;
				PlanetRadius = 0.9598409999999999;
				PlanetDiameterUncertainty_km = 2.0;
				break;
			case 4:
				PlanetName = "Mars   ";
				NumMoons = 2;
				NumLargeMoons = 0;
				PlanetRadius = 0.5320517;
				PlanetDiameterUncertainty_km = 0.4;
				break;
			case 5:
				PlanetName = "Jupiter";
				NumMoons = 13;
				NumLargeMoons = 4;
				PlanetRadius = (double)(float)Settings.Default.Radius_Jupiter / 6378.137;
				PlanetDiameterUncertainty_km = 12.0;
				break;
			case 6:
				PlanetName = "Saturn ";
				NumMoons = 14;
				NumLargeMoons = 8;
				PlanetRadius = (double)(float)Settings.Default.Radius_Saturn / 6378.137;
				PlanetDiameterUncertainty_km = 12.0;
				break;
			case 7:
				PlanetName = "Uranus ";
				NumMoons = 15;
				NumLargeMoons = 5;
				PlanetRadius = (double)(float)Settings.Default.Radius_Uranus / 6378.137;
				PlanetDiameterUncertainty_km = 14.0;
				break;
			case 8:
				PlanetName = "Neptune";
				NumMoons = 2;
				NumLargeMoons = 1;
				PlanetRadius = (double)(float)Settings.Default.Radius_Neptune / 6378.137;
				PlanetDiameterUncertainty_km = 38.0;
				break;
			case 9:
				PlanetName = "Pluto  ";
				NumMoons = 5;
				NumLargeMoons = 1;
				PlanetRadius = (double)(float)Settings.Default.Radius_Pluto / 6378.137;
				PlanetDiameterUncertainty_km = 1.6;
				break;
			case 3:
				break;
			}
		}

		internal static void PlanetSearch_Gaia(string GaiaCatBaseName, double StartJD, double EndJD, int Planet, bool IncludeLargeMoons, bool IncludeSmallMoons, bool UseHorizons, double LimitingStarMagForSearch, double MinimumDurationSet, double TelescopeAperture_cm, bool VarExposure, double ExpandedSearchDistInEarthRadii, bool AppendFlag, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			int num = 0;
			bool CloseEventFound = false;
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double RA2 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num2 = 0.0;
			long num3 = 0L;
			long num4 = 0L;
			long num5 = 0L;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			int num9 = 27;
			bool flag = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
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
			int num10 = (int)(fileStream2.Length / Gaia.RecordLength);
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum((int)(EndJD - StartJD + 1.0));
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			PlanetParameters(Planet, out var _, out var NumMoons, out var NumLargeMoons, out var PlanetRadius, out var _);
			for (int i = 0; i <= NumMoons; i++)
			{
				if (i > 0)
				{
					if ((!IncludeLargeMoons && !IncludeSmallMoons) || (IncludeLargeMoons && !IncludeSmallMoons && i > NumLargeMoons))
					{
						break;
					}
					if (!IncludeLargeMoons && IncludeSmallMoons && i <= NumLargeMoons)
					{
						continue;
					}
				}
				if (Planet == 5 && i == 5)
				{
					i++;
				}
				if (Planet == 6 && i == 10)
				{
					i = 12;
				}
				double num11 = LimitingStarMagForSearch;
				if (ShowProgressBar)
				{
					if ((i == 0) & CallingForm.chkPlanetMagLimit.get_Checked() & (num11 > PlanetMagLimit(Planet)))
					{
						num11 = PlanetMagLimit(Planet);
					}
				}
				else if ((i == 0) & (num11 > PlanetMagLimit(Planet)))
				{
					num11 = PlanetMagLimit(Planet);
				}
				for (double num12 = StartJD; num12 <= EndJD + 1.0; num12 += 1.0)
				{
					if (ShowProgressBar)
					{
						CallingForm.pbarSearch.set_Value((int)(num12 - StartJD));
					}
					if (num12 == StartJD)
					{
						if (i == 0)
						{
							Utilities.PlanetGeocentric(num12, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(Utilities.Planets[Planet]);
							}
						}
						else
						{
							Satellites.SatelliteCoordinates(num12, Planet, i, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
							GeocentricDist = Satellites.PlanetDistance;
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(MoonName);
							}
						}
						if (ShowProgressBar)
						{
							CallingForm.lstResults.get_Items().Clear();
						}
						num12 += 1.0;
					}
					Application.DoEvents();
					if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
					{
						AbortFlag = true;
					}
					double num13;
					double num14;
					if (i == 0)
					{
						Utilities.PlanetGeocentric(num12, Planet, 0.0, 2, out RA2, out Dec2, out GeocentricDist2);
						num13 = PlanetRadius;
						num14 = PlanetRadius * 6378.137 * 2.0;
					}
					else
					{
						Satellites.SatelliteCoordinates(num12, Planet, i, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
						GeocentricDist2 = Satellites.PlanetDistance;
						num13 = (double)(MoonDiaKm / 2f) / 6378.137;
						num14 = MoonDiaKm;
					}
					double num15 = (num12 - 2451545.0) / 365.25;
					double num16 = 0.002 + 0.0028 * (1.0 + num13) / GeocentricDist;
					double A = RA;
					double B = RA2;
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
					double num17 = 0.1 + 0.0028 * (1.0 + num13) / GeocentricDist;
					int num18 = 179 - (int)(360.0 / Math.PI * B2 + num17);
					if (B2 < 0.0)
					{
						num18++;
					}
					if (num18 < 0)
					{
						num18 = 0;
					}
					int num19 = 179 - (int)(360.0 / Math.PI * A2 - num17);
					if (A2 < 0.0)
					{
						num19++;
					}
					if (num19 < 0)
					{
						num19 = 0;
					}
					int num20 = 0;
					for (int j = num18; j <= num19; j++)
					{
						double num21 = (double)(180 - j) / 2.0;
						double num22 = num21 - 0.5;
						int num23 = 361 * j;
						int num24 = (int)Math.Floor(A * (180.0 / Math.PI) - 0.02);
						if (num24 < 0)
						{
							num24 += 360;
						}
						if (num24 > 360)
						{
							num24 -= 360;
						}
						int num25 = (int)Math.Ceiling(B * (180.0 / Math.PI) + 0.02);
						if (num25 > 360)
						{
							num25 -= 360;
						}
						if (num25 < 0)
						{
							num25 += 360;
						}
						fileStream.Seek(num23 * 4, SeekOrigin.Begin);
						array[0] = binaryReader.ReadInt32() - num9;
						if (array[0] < 0)
						{
							array[0] = 0;
						}
						fileStream.Seek((num23 + num24) * 4, SeekOrigin.Begin);
						array[1] = binaryReader.ReadInt32();
						fileStream.Seek((num23 + num25) * 4, SeekOrigin.Begin);
						array[2] = binaryReader.ReadInt32();
						fileStream.Seek((num23 + 360) * 4, SeekOrigin.Begin);
						array[3] = binaryReader.ReadInt32();
						int num26 = array[1];
						int num27 = array[2];
						double num28 = 0.0;
						if (num26 >= num27)
						{
							num26 = array[0];
							num27 = array[2];
							num28 = Math.PI * 2.0;
						}
						double num29 = B;
						double num30 = A - num28;
						do
						{
							IL_0630:
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
							if (num26 < num10)
							{
								if (!Gaia.ReadNext(fileStream2, binaryReader2, num26))
								{
									num26++;
								}
								else if (Gaia.gaiaVersionOfStar == 1 && ((Gaia.Dec_deg > num21) | (Gaia.Dec_deg < num22)))
								{
									num26++;
								}
								else
								{
									double RA3;
									double num31 = (RA3 = Gaia.RA_rad);
									double num32 = (num2 = Gaia.Dec_rad);
									double magGreen = Gaia.MagGreen;
									if (magGreen < num11 && RA3 > num30 - num16 && RA3 < num29 + num16 && num2 > A2 - num16 && num2 < B2 + num16)
									{
										double num33 = -2.5 * Math.Log10(Math.Pow(10.0, magGreen / -2.5) + Math.Pow(10.0, (double)Mag / -2.5));
										double num34 = (double)Mag - num33;
										bool flag2 = true;
										bool flag3 = (Planet == 5 && i > 0 && i < 5) || (Planet == 6 && ((i > 2 && i < 7) || i == 8));
										if ((i > 0 && !flag3) & (num34 < MagDropLimitForSatelliteSearches))
										{
											flag2 = false;
										}
										if (flag2)
										{
											long num35 = Gaia.StarNumber;
											if (num35 == 0 || (num35 != num3 && num35 != num4 && num35 != num5))
											{
												flag = true;
												if (LimitSearchToKepler2)
												{
													if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
													{
														Kepler2.Initialise_Kepler2_ForAsteroids();
													}
													flag = Kepler2.StarInKepler2(RA3 * (180.0 / Math.PI), num2 * (180.0 / Math.PI));
												}
												if (flag)
												{
													Utilities.ProperMotion(ref RA3, ref num2, Gaia.PMRA_rad, Gaia.PMDec_rad, Gaia.Parallax_rad, Gaia.RadialVelocityKmSec, num15 - Gaia.Epoch_2000);
													Gaia.Gaia_FrameRotationCorrections(Gaia.gaiaVersionOfStar, num12, RA3, num2, magGreen, out var dRA_asec, out var dDec_asec, out var _, out var _);
													RA3 += dRA_asec * Math.Cos(num2) / 1000.0 / (180.0 / Math.PI);
													num2 += dDec_asec / 1000.0 / (180.0 / Math.PI);
													BessellianElements(RA3, num2, RA, Dec, GeocentricDist, out var X, out var Y);
													BessellianElements(RA3, num2, RA2, Dec2, GeocentricDist2, out var X2, out var Y2);
													double num36 = X2 - X;
													double num37 = Y2 - Y;
													double num38 = Math.Sqrt(num36 * num36 + num37 * num37);
													double num39 = X * num36 + Y * num37;
													double value = (X * num37 - Y * num36) / num38;
													double num40 = Math.Floor((0.0 - num39) / num38 / num38 * 24.0);
													double num41 = num14 / num38 * 86400.0 / 6378.137;
													bool flag4 = true;
													SingleExposureMultiplierToGetToMag = 1.0;
													NumberOfExposuresRequired_ToBeSure = 3.0;
													if (TelescopeAperture_cm > 0.0)
													{
														double num42 = 5.5 + 5.0 * Math.Log10(TelescopeAperture_cm);
														if (VarExposure && Mag > 5f)
														{
															if (magGreen > num42)
															{
																SingleExposureMultiplierToGetToMag = Math.Pow(2.512, magGreen - num42);
															}
															else
															{
																SingleExposureMultiplierToGetToMag = 1.0;
															}
															if (num34 <= 0.4)
															{
																NumberOfExposuresRequired_ToBeSure = 0.75 / num34 / num34;
															}
															flag4 = num41 > 0.0367 * SingleExposureMultiplierToGetToMag * NumberOfExposuresRequired_ToBeSure;
														}
														else
														{
															flag4 = (magGreen < num42) & (num41 > 0.0367 * NumberOfExposuresRequired_ToBeSure);
														}
													}
													if (((num41 > MinimumDurationSet && flag4) & (Math.Abs(value) < 15.0 + ExpandedSearchDistInEarthRadii)) && num40 > -5.0 && num40 < 29.0)
													{
														string starID = Gaia.StarID;
														double parallax_rad = Gaia.Parallax_rad;
														double magGreen2 = Gaia.MagGreen;
														double magBlue = Gaia.MagBlue;
														double num43 = Gaia.SigmaRA_arcsecs(num15);
														double num44 = Gaia.SigmaDec_arcsecs(num15);
														num6 = Math.Sqrt(0.05 * 0.05 + num43 * num43);
														num7 = Math.Sqrt(0.05 * 0.05 + num44 * num44);
														num8 = 90.0;
														string errorBasis = "Star+Assumed";
														double starDiaMAS = Gaia.StarDiameter_mas;
														string Basis;
														int NumMeasures;
														bool InvalidDiameter;
														double num45 = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS((num31 - Gaia.PMRA_rad * Gaia.Epoch_2000) * (180.0 / Math.PI) / 15.0, (num32 - Gaia.PMDec_rad * Gaia.Epoch_2000) * (180.0 / Math.PI), magGreen, magBlue, magGreen2, out Basis, out NumMeasures, out InvalidDiameter);
														if (num45 > 1.0)
														{
															starDiaMAS = num45;
														}
														double starReliability = Gaia.Reliability;
														if (Gaia.HipPositionUnreliable_UBSC)
														{
															starReliability = -2.0;
														}
														int noGaiaProperMotion = Gaia.NoGaiaProperMotion;
														int properMotionUsingUCAC = Gaia.ProperMotionUsingUCAC4;
														int duplicateSource = Gaia.DuplicateSource;
														PlanetPrediction(RA3, num2, parallax_rad, starID, magGreen, magBlue, magGreen2, num6, num7, num8, errorBasis, num12 - 1.0, num40, ExpandedSearchDistInEarthRadii, out CloseEventFound, Planet, i, UseHorizons, starDiaMAS, starReliability, noGaiaProperMotion, properMotionUsingUCAC, duplicateSource, num13, AutoSaveSearch, OutputFileName);
														num++;
														if (CloseEventFound)
														{
															num5 = num4;
															num4 = num3;
															num3 = num35;
															num20++;
														}
													}
												}
											}
										}
									}
									num26++;
								}
								if (num26 <= num27)
								{
									goto IL_0630;
								}
							}
							if (num28 == 0.0)
							{
								break;
							}
							num28 = 0.0;
							num26 = array[1];
							num27 = array[3];
							num29 += Math.PI * 2.0;
							num30 += Math.PI * 2.0;
						}
						while (num28 >= 0.0);
					}
					RA = RA2;
					Dec = Dec2;
					GeocentricDist = GeocentricDist2;
					if (num20 == 0)
					{
						num5 = num4;
						num4 = num3;
						num3 = 0L;
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

		internal static void PlanetSearch_Tycho2(double StartJD, double EndJD, int Planet, bool IncludeLargeMoons, bool IncludeSmallMoons, bool UseHorizons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, bool UseTycho2, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar, string StarCatalogueBase)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool CloseEventFound = false;
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double RA2 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			int num9 = 27;
			bool flag = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
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
				CallingForm.pbarSearch.set_Maximum((int)(EndJD - StartJD + 1.0));
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			PlanetParameters(Planet, out var _, out var NumMoons, out var NumLargeMoons, out var PlanetRadius, out var _);
			for (int i = 0; i <= NumMoons; i++)
			{
				if (i > 0)
				{
					if ((!IncludeLargeMoons && !IncludeSmallMoons) || (IncludeLargeMoons && !IncludeSmallMoons && i > NumLargeMoons))
					{
						break;
					}
					if (!IncludeLargeMoons && IncludeSmallMoons && i <= NumLargeMoons)
					{
						continue;
					}
				}
				if (Planet == 5 && i == 5)
				{
					i++;
				}
				if (Planet == 6 && i == 10)
				{
					i = 12;
				}
				double num11 = LimitingStarMagForSearch;
				if (ShowProgressBar)
				{
					if ((i == 0) & CallingForm.chkPlanetMagLimit.get_Checked())
					{
						num11 = PlanetMagLimit(Planet);
					}
				}
				else if (i == 0)
				{
					num11 = PlanetMagLimit(Planet);
				}
				for (double num12 = StartJD; num12 <= EndJD + 1.0; num12 += 1.0)
				{
					if (ShowProgressBar)
					{
						CallingForm.pbarSearch.set_Value((int)(num12 - StartJD));
					}
					if (num12 == StartJD)
					{
						if (i == 0)
						{
							Utilities.PlanetGeocentric(num12, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(Utilities.Planets[Planet]);
							}
						}
						else
						{
							Satellites.SatelliteCoordinates(num12, Planet, i, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
							GeocentricDist = Satellites.PlanetDistance;
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(MoonName);
							}
						}
						if (ShowProgressBar)
						{
							CallingForm.lstResults.get_Items().Clear();
						}
						num12 += 1.0;
					}
					Application.DoEvents();
					if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
					{
						AbortFlag = true;
					}
					double num13;
					if (i == 0)
					{
						Utilities.PlanetGeocentric(num12, Planet, 0.0, 2, out RA2, out Dec2, out GeocentricDist2);
						num13 = PlanetRadius;
					}
					else
					{
						Satellites.SatelliteCoordinates(num12, Planet, i, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
						GeocentricDist2 = Satellites.PlanetDistance;
						num13 = (double)(MoonDiaKm / 2f) / 6378.137;
					}
					double num14 = (num12 - 2451545.0) / 365.25;
					double num15 = 0.002 + 0.0028 * (1.0 + num13) / GeocentricDist;
					double A = RA;
					double B = RA2;
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
					double num16 = 0.1 + 0.0028 * (1.0 + num13) / GeocentricDist;
					int num17 = 89 - (int)(180.0 / Math.PI * B2 + num16);
					if (B2 < 0.0)
					{
						num17++;
					}
					if (num17 < 0)
					{
						num17 = 0;
					}
					int num18 = 89 - (int)(180.0 / Math.PI * A2 - num16);
					if (A2 < 0.0)
					{
						num18++;
					}
					if (num18 < 0)
					{
						num18 = 0;
					}
					int num19 = 0;
					for (int j = num17; j <= num18; j++)
					{
						int num20 = 361 * j;
						int num21 = (int)Math.Floor(A * (180.0 / Math.PI) - 0.02);
						if (num21 < 0)
						{
							num21 += 360;
						}
						if (num21 > 360)
						{
							num21 -= 360;
						}
						int num22 = (int)Math.Ceiling(B * (180.0 / Math.PI) + 0.02);
						if (num22 > 360)
						{
							num22 -= 360;
						}
						if (num22 < 0)
						{
							num22 += 360;
						}
						fileStream.Seek(num20 * 4, SeekOrigin.Begin);
						array[0] = binaryReader.ReadInt32() - num9;
						if (array[0] < 0)
						{
							array[0] = 0;
						}
						fileStream.Seek((num20 + num21) * 4, SeekOrigin.Begin);
						array[1] = binaryReader.ReadInt32();
						fileStream.Seek((num20 + num22) * 4, SeekOrigin.Begin);
						array[2] = binaryReader.ReadInt32();
						fileStream.Seek((num20 + 360) * 4, SeekOrigin.Begin);
						array[3] = binaryReader.ReadInt32();
						int num23 = array[1];
						int num24 = array[2];
						double num25 = 0.0;
						if (num23 >= num24)
						{
							num23 = array[0];
							num24 = array[2];
							num25 = Math.PI * 2.0;
						}
						double num26 = B;
						double num27 = A - num25;
						do
						{
							IL_05fe:
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
							if (num23 < num10)
							{
								Tycho2.ReadNext(fileStream2, binaryReader2, num23);
								double rA;
								double num28 = (rA = Tycho2.RA);
								double num29 = num14 * Tycho2.PMRA;
								double num30 = (num5 = Tycho2.Dec);
								if (rA > num27 - num15 && rA < num26 + num15 && num5 > A2 - num15 && num5 < B2 + num15)
								{
									double num31 = num14 * Tycho2.PMDec;
									double piStar = Tycho2.Parallax_asec / 3600.0 / (180.0 / Math.PI);
									double magV = Tycho2.MagV;
									double magB = Tycho2.MagB;
									double num32 = -2.5 * Math.Log10(Math.Pow(10.0, magV / -2.5) + Math.Pow(10.0, (double)Mag / -2.5));
									bool flag2 = true;
									if ((i > 0) & ((double)Mag - num32 < MagDropLimitForSatelliteSearches))
									{
										flag2 = false;
									}
									if (magV <= num11 && flag2)
									{
										int starNumber = Tycho2.StarNumber;
										if (starNumber != num && starNumber != num2 && starNumber != num3)
										{
											rA += num29;
											num5 += num31;
											flag = true;
											if (LimitSearchToKepler2)
											{
												if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
												{
													Kepler2.Initialise_Kepler2_ForAsteroids();
												}
												flag = Kepler2.StarInKepler2(rA * (180.0 / Math.PI), num5 * (180.0 / Math.PI));
											}
											if (flag)
											{
												BessellianElements(rA, num5, RA, Dec, GeocentricDist, out var X, out var Y);
												BessellianElements(rA, num5, RA2, Dec2, GeocentricDist2, out var X2, out var Y2);
												double num33 = X2 - X;
												double num34 = Y2 - Y;
												double num35 = Math.Sqrt(num33 * num33 + num34 * num34);
												double num36 = X * num33 + Y * num34;
												double value = (X * num34 - Y * num33) / num35;
												double num37 = Math.Floor((0.0 - num36) / num35 / num35 * 24.0);
												if (Math.Abs(value) < 10.0 + ExpandedSearchDistInEarthRadii && num37 > -5.0 && num37 < 29.0)
												{
													string starID = Tycho2.StarID;
													double num38 = Tycho2.ErrorRA_arcsecs(2000.0 + num14);
													double num39 = Tycho2.ErrorDec_arcsecs(2000.0 + num14);
													num6 = Math.Sqrt(0.05 * 0.05 + num38 * num38);
													num7 = Math.Sqrt(0.05 * 0.05 + num39 * num39);
													num8 = 90.0;
													string errorBasis = "Star+Assumed";
													string Basis;
													int NumMeasures;
													bool InvalidDiameter;
													double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num28 * (180.0 / Math.PI) / 15.0, num30 * (180.0 / Math.PI), magV, magB, 0.0, out Basis, out NumMeasures, out InvalidDiameter);
													if (InvalidDiameter)
													{
														starDiaMAS = Gaia.StarDiameter_mas;
													}
													PlanetPrediction(rA, num5, piStar, starID, magV, magB, 0.0, num6, num7, num8, errorBasis, num12 - 1.0, num37, ExpandedSearchDistInEarthRadii, out CloseEventFound, Planet, i, UseHorizons, starDiaMAS, num13, AutoSaveSearch, OutputFileName);
													num4++;
													if (CloseEventFound)
													{
														num3 = num2;
														num2 = num;
														num = starNumber;
														num19++;
													}
												}
											}
										}
									}
								}
								num23++;
								if (num23 <= num24)
								{
									goto IL_05fe;
								}
							}
							if (num25 == 0.0)
							{
								break;
							}
							num25 = 0.0;
							num23 = array[1];
							num24 = array[3];
							num26 += Math.PI * 2.0;
							num27 += Math.PI * 2.0;
						}
						while (num25 >= 0.0);
					}
					RA = RA2;
					Dec = Dec2;
					GeocentricDist = GeocentricDist2;
					if (num19 == 0)
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

		internal static void PlanetSearch_UCAC4(double StartJD, double EndJD, int Planet, bool IncludeLargeMoons, bool IncludeSmallMoons, bool UseHorizons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool CloseEventFound = false;
			bool flag = false;
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double RA2 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num5 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			_ = new byte[12];
			bool flag2 = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			UCAC4.InitialiseUCAC4();
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum((int)(EndJD - StartJD + 1.0));
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			PlanetParameters(Planet, out var _, out var NumMoons, out var NumLargeMoons, out var PlanetRadius, out var _);
			for (int i = 0; i <= NumMoons; i++)
			{
				if (i > 0)
				{
					if ((!IncludeLargeMoons && !IncludeSmallMoons) || (IncludeLargeMoons && !IncludeSmallMoons && i > NumLargeMoons))
					{
						break;
					}
					if (!IncludeLargeMoons && IncludeSmallMoons && i <= NumLargeMoons)
					{
						continue;
					}
				}
				if (Planet == 5 && i == 5)
				{
					i++;
				}
				if (Planet == 6 && i == 10)
				{
					i = 12;
				}
				double num6 = LimitingStarMagForSearch;
				if (ShowProgressBar)
				{
					if ((i == 0) & CallingForm.chkPlanetMagLimit.get_Checked())
					{
						num6 = PlanetMagLimit(Planet);
					}
				}
				else if (i == 0)
				{
					num6 = PlanetMagLimit(Planet);
				}
				for (double num7 = StartJD; num7 <= EndJD + 1.0; num7 += 1.0)
				{
					if (ShowProgressBar)
					{
						CallingForm.pbarSearch.set_Value((int)(num7 - StartJD));
					}
					if (num7 == StartJD)
					{
						if (i == 0)
						{
							Utilities.PlanetGeocentric(num7, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(Utilities.Planets[Planet]);
							}
						}
						else
						{
							Satellites.SatelliteCoordinates(num7, Planet, i, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
							GeocentricDist = Satellites.PlanetDistance;
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(MoonName);
							}
						}
						if (ShowProgressBar)
						{
							CallingForm.lstResults.get_Items().Clear();
						}
						num7 += 1.0;
					}
					Application.DoEvents();
					if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
					{
						AbortFlag = true;
					}
					double num8;
					if (i == 0)
					{
						Utilities.PlanetGeocentric(num7, Planet, 0.0, 2, out RA2, out Dec2, out GeocentricDist2);
						num8 = PlanetRadius;
					}
					else
					{
						Satellites.SatelliteCoordinates(num7, Planet, i, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
						GeocentricDist2 = Satellites.PlanetDistance;
						num8 = (double)(MoonDiaKm / 2f) / 6378.137;
					}
					double num9 = (num7 - 2451545.0) / 365.25;
					double num10 = 0.002 + 0.0028 * (1.0 + num8) / GeocentricDist;
					double A = RA;
					double B = RA2;
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
					double num11 = 0.1 + 0.0028 * (1.0 + num8) / GeocentricDist;
					int num12 = (int)(451.0 + 900.0 / Math.PI * B2 + num11);
					if (num12 < 1)
					{
						num12 = 1;
					}
					int num13 = (int)(451.0 + 900.0 / Math.PI * A2 - num11);
					if (num13 < 1)
					{
						num13 = 1;
					}
					int num14 = 0;
					for (int j = num13; j <= num12 && j <= 900; j++)
					{
						int num15 = (int)Math.Floor(A * (180.0 / Math.PI) * 4.0 - 0.02);
						if (num15 < 0)
						{
							num15 += 1440;
						}
						if (num15 > 1439)
						{
							num15 -= 1440;
						}
						int num16 = (int)Math.Ceiling(B * (180.0 / Math.PI) * 4.0 + 0.02);
						if (num16 < 0)
						{
							num16 += 1440;
						}
						if (num16 > 1439)
						{
							num16 -= 1440;
						}
						UCAC4.Open_UCAC4_Catalogue(j);
						array[0] = 0;
						UCAC4.GetUCAC4IndexAndBin(j, num15, out var StartRecordNum, out var NumInBin);
						array[1] = StartRecordNum;
						UCAC4.GetUCAC4IndexAndBin(j, num16, out StartRecordNum, out NumInBin);
						array[2] = StartRecordNum + NumInBin;
						UCAC4.GetUCAC4IndexAndBin(j, 1439, out StartRecordNum, out NumInBin);
						array[3] = StartRecordNum + NumInBin;
						int num17 = array[1];
						int num18 = array[2];
						double num19 = 0.0;
						if (num17 >= num18)
						{
							num17 = array[0];
							num18 = array[2];
							num19 = Math.PI * 2.0;
						}
						double num20 = B;
						double num21 = A - num19;
						while (true)
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
							int num22 = num17 + 1;
							if (num22 != num && num22 != num2 && num22 != num3)
							{
								UCAC4.Read_UCAC4_entry(num17, UseHipparcosForParallax: false);
								if (flag | !UCAC4.DoubtfulObject)
								{
									double rA;
									double num23 = (rA = UCAC4.RA);
									double num24 = (num5 = UCAC4.Dec);
									if (rA > num21 - num10 && rA < num20 + num10 && num5 > A2 - num10 && num5 < B2 + num10)
									{
										double magV = UCAC4.MagV;
										double num25 = num9 * UCAC4.PM_ra;
										double num26 = num9 * UCAC4.PM_dec;
										double num27 = -2.5 * Math.Log10(Math.Pow(10.0, magV / -2.5) + Math.Pow(10.0, (double)Mag / -2.5));
										bool flag3 = true;
										if ((i > 0) & ((double)Mag - num27 < MagDropLimitForSatelliteSearches))
										{
											flag3 = false;
										}
										if (magV <= num6 && flag3)
										{
											rA += num25;
											num5 += num26;
											double piStar = UCAC4.Parallax_mas / 1000.0 / 3600.0 / (180.0 / Math.PI);
											flag2 = true;
											if (LimitSearchToKepler2)
											{
												if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
												{
													Kepler2.Initialise_Kepler2_ForAsteroids();
												}
												flag2 = Kepler2.StarInKepler2(rA * (180.0 / Math.PI), num5 * (180.0 / Math.PI));
											}
											if (flag2)
											{
												BessellianElements(rA, num5, RA, Dec, GeocentricDist, out var X, out var Y);
												BessellianElements(rA, num5, RA2, Dec2, GeocentricDist2, out var X2, out var Y2);
												double num28 = X2 - X;
												double num29 = Y2 - Y;
												double num30 = Math.Sqrt(num28 * num28 + num29 * num29);
												double num31 = X * num28 + Y * num29;
												double value = (X * num29 - Y * num28) / num30;
												double num32 = Math.Floor((0.0 - num31) / num30 / num30 * 24.0);
												if (Math.Abs(value) < 10.0 + ExpandedSearchDistInEarthRadii && num32 > -5.0 && num32 < 29.0)
												{
													double num33 = UCAC4.ErrorRA_arcsecs(2000.0 + num9);
													double num34 = UCAC4.ErrorDec_arcsecs(2000.0 + num9);
													double errorEllipseMajor = Math.Sqrt(0.05 * 0.05 + num33 * num33);
													double errorEllipseMinor = Math.Sqrt(0.05 * num34 * num34);
													double errorEllipsePA = 90.0;
													string errorBasis = "Star+Assumed";
													string uCACnumber = UCAC4.UCACnumber;
													double magB = UCAC4.MagB;
													double magR = UCAC4.MagR;
													string Basis;
													int NumMeasures;
													bool InvalidDiameter;
													double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num23 * (180.0 / Math.PI) / 15.0, num24 * (180.0 / Math.PI), magV, magB, magR, out Basis, out NumMeasures, out InvalidDiameter);
													if (InvalidDiameter)
													{
														starDiaMAS = Gaia.StarDiameter_mas;
													}
													PlanetPrediction(rA, num5, piStar, uCACnumber, magV, magB, magR, errorEllipseMajor, errorEllipseMinor, errorEllipsePA, errorBasis, num7 - 1.0, num32, ExpandedSearchDistInEarthRadii, out CloseEventFound, Planet, i, UseHorizons, starDiaMAS, num8, AutoSaveSearch, OutputFileName);
													num4++;
													if (CloseEventFound)
													{
														num3 = num2;
														num2 = num;
														num = num22;
														num14++;
													}
												}
											}
										}
									}
								}
							}
							num17++;
							if (num17 > num18)
							{
								if (num19 == 0.0)
								{
									break;
								}
								num19 = 0.0;
								num17 = array[1];
								num18 = array[3];
								num20 += Math.PI * 2.0;
								num21 += Math.PI * 2.0;
								if (!(num19 >= 0.0))
								{
									break;
								}
							}
						}
						UCAC4.Close_UCAC4_Catalogue();
					}
					RA = RA2;
					Dec = Dec2;
					GeocentricDist = GeocentricDist2;
					if (num14 == 0)
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

		internal static void PlanetSearch_PPMXL(double StartJD, double EndJD, int Planet, bool IncludeLargeMoons, bool IncludeSmallMoons, bool UseHorizons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool CloseEventFound = false;
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double RA2 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num5 = 0.0;
			int[] array = new int[6];
			_ = new string[5];
			_ = new byte[12];
			bool flag = true;
			if (EndJD < StartJD)
			{
				return;
			}
			if (LimitingStarMagForSearch < 4.0)
			{
				LimitingStarMagForSearch = 12.0;
			}
			PPMXL.InitialisePPMXL();
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum((int)(EndJD - StartJD + 1.0));
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			PlanetParameters(Planet, out var _, out var NumMoons, out var NumLargeMoons, out var PlanetRadius, out var _);
			for (int i = 0; i <= NumMoons; i++)
			{
				if (i > 0)
				{
					if ((!IncludeLargeMoons && !IncludeSmallMoons) || (IncludeLargeMoons && !IncludeSmallMoons && i > NumLargeMoons))
					{
						break;
					}
					if (!IncludeLargeMoons && IncludeSmallMoons && i <= NumLargeMoons)
					{
						continue;
					}
				}
				if (Planet == 5 && i == 5)
				{
					i++;
				}
				if (Planet == 6 && i == 10)
				{
					i = 12;
				}
				double num6 = LimitingStarMagForSearch;
				if (ShowProgressBar)
				{
					if ((i == 0) & CallingForm.chkPlanetMagLimit.get_Checked())
					{
						num6 = PlanetMagLimit(Planet);
					}
				}
				else if (i == 0)
				{
					num6 = PlanetMagLimit(Planet);
				}
				for (double num7 = StartJD; num7 <= EndJD + 1.0; num7 += 1.0)
				{
					if (ShowProgressBar)
					{
						CallingForm.pbarSearch.set_Value((int)(num7 - StartJD));
					}
					if (num7 == StartJD)
					{
						if (i == 0)
						{
							Utilities.PlanetGeocentric(num7, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(Utilities.Planets[Planet]);
							}
						}
						else
						{
							Satellites.SatelliteCoordinates(num7, Planet, i, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
							GeocentricDist = Satellites.PlanetDistance;
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(MoonName);
							}
						}
						if (ShowProgressBar)
						{
							CallingForm.lstResults.get_Items().Clear();
						}
						num7 += 1.0;
					}
					Application.DoEvents();
					if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
					{
						AbortFlag = true;
					}
					double num8;
					if (i == 0)
					{
						Utilities.PlanetGeocentric(num7, Planet, 0.0, 2, out RA2, out Dec2, out GeocentricDist2);
						num8 = PlanetRadius;
					}
					else
					{
						Satellites.SatelliteCoordinates(num7, Planet, i, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
						GeocentricDist2 = Satellites.PlanetDistance;
						num8 = (double)(MoonDiaKm / 2f) / 6378.137;
					}
					double num9 = (num7 - 2451545.0) / 365.25;
					double num10 = 0.002 + 0.0028 * (1.0 + num8) / GeocentricDist;
					double A = RA;
					double B = RA2;
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
					double num11 = 0.1 + 0.0028 * (1.0 + num8) / GeocentricDist;
					int num12 = (int)(361.0 + 720.0 / Math.PI * B2 + num11);
					if (num12 < 0)
					{
						num12 = 0;
					}
					int num13 = (int)(361.0 + 720.0 / Math.PI * A2 - num11);
					if (num13 < 0)
					{
						num13 = 0;
					}
					int num14 = 0;
					for (int j = num13; j <= num12 && j <= 720; j++)
					{
						int num15 = (int)Math.Floor(A * (180.0 / Math.PI) * 5.0 - 0.02);
						if (num15 < 0)
						{
							num15 += 1800;
						}
						if (num15 > 1800)
						{
							num15 -= 1800;
						}
						int num16 = (int)Math.Ceiling(B * (180.0 / Math.PI) * 5.0 + 0.02);
						if (num16 < 0)
						{
							num16 += 1800;
						}
						if (num16 > 1800)
						{
							num16 -= 1800;
						}
						PPMXL.Open_PPMXL_Catalogue(j);
						array[0] = PPMXL.GetPPMXLIndexValue(j, 0);
						array[1] = PPMXL.GetPPMXLIndexValue(j, num15);
						array[2] = PPMXL.GetPPMXLIndexValue(j, num16);
						array[3] = PPMXL.GetPPMXLIndexValue(j, 1800);
						int num17 = array[1];
						int num18 = array[2];
						double num19 = 0.0;
						if (num17 >= num18)
						{
							num17 = array[0];
							num18 = array[2];
							num19 = Math.PI * 2.0;
						}
						double num20 = B;
						double num21 = A - num19;
						while (true)
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
							int num22 = num17 + 1;
							if (num22 != num && num22 != num2 && num22 != num3)
							{
								PPMXL.Read_PPMXL_entry(num17);
								double rA_J;
								double num23 = (rA_J = PPMXL.RA_J2000);
								double num24 = (num5 = PPMXL.Dec_J2000);
								if (rA_J > num21 - num10 && rA_J < num20 + num10 && num5 > A2 - num10 && num5 < B2 + num10)
								{
									double mag_V = PPMXL.mag_V;
									double num25 = num9 * PPMXL.PM_RA;
									double num26 = num9 * PPMXL.PM_Dec;
									double num27 = -2.5 * Math.Log10(Math.Pow(10.0, mag_V / -2.5) + Math.Pow(10.0, (double)Mag / -2.5));
									bool flag2 = true;
									if ((i > 0) & ((double)Mag - num27 < MagDropLimitForSatelliteSearches))
									{
										flag2 = false;
									}
									if (mag_V <= num6 && flag2)
									{
										rA_J += num25;
										num5 += num26;
										flag = true;
										if (LimitSearchToKepler2)
										{
											if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
											{
												Kepler2.Initialise_Kepler2_ForAsteroids();
											}
											flag = Kepler2.StarInKepler2(rA_J * (180.0 / Math.PI), num5 * (180.0 / Math.PI));
										}
										if (flag)
										{
											BessellianElements(rA_J, num5, RA, Dec, GeocentricDist, out var X, out var Y);
											BessellianElements(rA_J, num5, RA2, Dec2, GeocentricDist2, out var X2, out var Y2);
											double num28 = X2 - X;
											double num29 = Y2 - Y;
											double num30 = Math.Sqrt(num28 * num28 + num29 * num29);
											double num31 = X * num28 + Y * num29;
											double value = (X * num29 - Y * num28) / num30;
											double num32 = Math.Floor((0.0 - num31) / num30 / num30 * 24.0);
											if (Math.Abs(value) < 10.0 + ExpandedSearchDistInEarthRadii && num32 > -5.0 && num32 < 29.0)
											{
												double num33 = PPMXL.ErrorRA_arcsecs(2000.0 + num9);
												double num34 = PPMXL.ErrorDec_arcsecs(2000.0 + num9);
												double errorEllipseMajor = Math.Sqrt(0.05 * 0.05 + num33 * num33);
												double errorEllipseMinor = Math.Sqrt(0.05 * 0.05 + num34 * num34);
												double errorEllipsePA = 90.0;
												string errorBasis = "Star+Assumed";
												string pPMXnumber = PPMXL.PPMXnumber;
												double mag_B = PPMXL.mag_B;
												double mag_R = PPMXL.mag_R;
												string Basis;
												int NumMeasures;
												bool InvalidDiameter;
												double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(num23 * (180.0 / Math.PI) / 15.0, num24 * (180.0 / Math.PI), mag_V, mag_B, mag_R, out Basis, out NumMeasures, out InvalidDiameter);
												PlanetPrediction(rA_J, num5, 0.0, pPMXnumber, mag_V, mag_B, mag_R, errorEllipseMajor, errorEllipseMinor, errorEllipsePA, errorBasis, num7 - 1.0, num32, ExpandedSearchDistInEarthRadii, out CloseEventFound, Planet, i, UseHorizons, starDiaMAS, num8, AutoSaveSearch, OutputFileName);
												num4++;
												if (CloseEventFound)
												{
													num3 = num2;
													num2 = num;
													num = num22;
													num14++;
												}
											}
										}
									}
								}
							}
							num17++;
							if (num17 > num18)
							{
								if (num19 == 0.0)
								{
									break;
								}
								num19 = 0.0;
								num17 = array[1];
								num18 = array[3];
								num20 += Math.PI * 2.0;
								num21 += Math.PI * 2.0;
								if (!(num19 >= 0.0))
								{
									break;
								}
							}
						}
						PPMXL.Close_PPMXL_Catalogue();
					}
					RA = RA2;
					Dec = Dec2;
					GeocentricDist = GeocentricDist2;
					if (num14 == 0)
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

		internal static void PlanetSearch_UserStar(double StartJD, double EndJD, int Planet, bool IncludeLargeMoons, bool IncludeSmallMoons, bool UseHorizons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, string UserStarCatFile, bool AutoSaveSearch, string OutputFileName, bool ShowProgressBar)
		{
			bool CloseEventFound = false;
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double RA2 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num = 0.0;
			double num2 = 2000.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double starReliability = -1.0;
			int noGaiaPM = -1;
			int gaiaUCAC4PM = 0;
			int duplicateSource = 0;
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
			double num5;
			double muRA_Annual_Rad;
			double num6;
			double muDec_Annual_Rad;
			double num7;
			double magV;
			double magR;
			double magB;
			if (text.Substring(21).Contains(","))
			{
				string[] array = text.Split(new char[1] { ',' });
				starCatName = array[0].Trim();
				num2 = double.Parse(array[1]);
				num5 = (double.Parse(array[2]) + double.Parse(array[3]) / 60.0 + double.Parse(array[4]) / 3600.0) * 15.0 / (180.0 / Math.PI);
				muRA_Annual_Rad = double.Parse(array[5]) / 3600.0 * 15.0 / (180.0 / Math.PI);
				num6 = (double.Parse(array[6].Replace("-", "").Replace("+", "")) + double.Parse(array[7]) / 60.0 + double.Parse(array[8]) / 3600.0) / (180.0 / Math.PI);
				if (array[6].Contains("-"))
				{
					num6 = 0.0 - num6;
				}
				muDec_Annual_Rad = double.Parse(array[9]) / 3600.0 / (180.0 / Math.PI);
				num7 = double.Parse(array[10]) / 3600.0 / (180.0 / Math.PI);
				num4 = double.Parse(array[11]);
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
				num5 = (double.Parse(text.Substring(21, 2)) + double.Parse(text.Substring(23, 2)) / 60.0 + double.Parse(text.Substring(25, 7)) / 3600.0) * 15.0 / (180.0 / Math.PI);
				muRA_Annual_Rad = double.Parse(text.Substring(32, 8)) / 3600.0 * 15.0 / (180.0 / Math.PI);
				num6 = (double.Parse(text.Substring(43, 2)) + double.Parse(text.Substring(45, 2)) / 60.0 + double.Parse(text.Substring(47, 6)) / 3600.0) / (180.0 / Math.PI);
				if (text.Substring(42, 1) == "-")
				{
					num = 0.0 - num;
				}
				muDec_Annual_Rad = double.Parse(text.Substring(53, 7)) / 3600.0 / (180.0 / Math.PI);
				num7 = double.Parse(text.Substring(62, 6)) / 3600.0 / (180.0 / Math.PI);
				num4 = 0.0;
				magV = double.Parse(text.Substring(69, 5));
				magR = 0.0;
				magB = double.Parse(text.Substring(75, 5));
			}
			if (ShowProgressBar)
			{
				CallingForm.pbarSearch.set_Minimum(0);
				CallingForm.pbarSearch.set_Maximum((int)(EndJD - StartJD + 1.0));
				((Control)CallingForm.pbarSearch).set_Visible(true);
				((Control)CallingForm.cmdCompute).set_Visible(false);
			}
			AbortFlag = false;
			PlanetParameters(Planet, out var _, out var NumMoons, out var NumLargeMoons, out var PlanetRadius, out var _);
			for (int i = 0; i <= NumMoons; i++)
			{
				if (i > 0)
				{
					if ((!IncludeLargeMoons && !IncludeSmallMoons) || (IncludeLargeMoons && !IncludeSmallMoons && i > NumLargeMoons))
					{
						break;
					}
					if (!IncludeLargeMoons && IncludeSmallMoons && i <= NumLargeMoons)
					{
						continue;
					}
				}
				if (Planet == 5 && i == 5)
				{
					i++;
				}
				if (Planet == 6 && i == 10)
				{
					i = 12;
				}
				if (ShowProgressBar)
				{
					if ((i == 0) & CallingForm.chkPlanetMagLimit.get_Checked())
					{
						PlanetMagLimit(Planet);
					}
				}
				else if (i == 0)
				{
					PlanetMagLimit(Planet);
				}
				for (double num8 = StartJD; num8 <= EndJD + 1.0; num8 += 1.0)
				{
					if (ShowProgressBar)
					{
						CallingForm.pbarSearch.set_Value((int)(num8 - StartJD));
					}
					if (num8 == StartJD)
					{
						if (i == 0)
						{
							Utilities.PlanetGeocentric(num8, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(Utilities.Planets[Planet]);
							}
						}
						else
						{
							Satellites.SatelliteCoordinates(num8, Planet, i, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
							GeocentricDist = Satellites.PlanetDistance;
							if (ShowProgressBar)
							{
								((Control)CallingForm.lblAsteroidName).set_Text(MoonName);
							}
						}
						if (ShowProgressBar)
						{
							CallingForm.lstResults.get_Items().Clear();
						}
						num8 += 1.0;
					}
					Application.DoEvents();
					if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
					{
						AbortFlag = true;
					}
					if (AbortFlag && ShowProgressBar)
					{
						((Control)CallingForm.pbarSearch).set_Visible(false);
						((Control)CallingForm.cmdCompute).set_Visible(true);
						CallingForm.SearchCancelled = true;
						return;
					}
					double num9;
					if (i == 0)
					{
						Utilities.PlanetGeocentric(num8, Planet, 0.0, 2, out RA2, out Dec2, out GeocentricDist2);
						num9 = PlanetRadius;
					}
					else
					{
						Satellites.SatelliteCoordinates(num8, Planet, i, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
						GeocentricDist2 = Satellites.PlanetDistance;
						num9 = (double)(MoonDiaKm / 2f) / 6378.137;
					}
					num3 = (num8 - 2451545.0) / 365.25;
					double num10 = 0.002 + 0.0028 * (1.0 + num9) / GeocentricDist;
					double RA3 = num5;
					num = num6;
					Utilities.ProperMotion(ref RA3, ref num, muRA_Annual_Rad, muDec_Annual_Rad, num7, num4, num3 + 2000.0 - num2);
					double A = RA;
					double B = RA2;
					if (Math.Abs(B - A) < Math.PI)
					{
						if (A > B)
						{
							Utilities.Swap(ref A, ref B);
						}
					}
					else
					{
						if (A < B)
						{
							Utilities.Swap(ref A, ref B);
						}
						if (RA3 < 0.2 && A > 6.0 && B < 0.3)
						{
							A -= Math.PI * 2.0;
						}
						if (RA3 > 6.0 && A > 6.0 && B < 0.3)
						{
							B += Math.PI * 2.0;
						}
					}
					double A2 = Dec;
					double B2 = Dec2;
					if (A2 > B2)
					{
						Utilities.Swap(ref A2, ref B2);
					}
					_ = 0.0028 * (1.0 + num9) / GeocentricDist;
					if (!(RA3 > A - num10 && RA3 < B + num10 && num > A2 - num10 && num < B2 + num10))
					{
						continue;
					}
					BessellianElements(RA3, num, RA, Dec, GeocentricDist, out var X, out var Y);
					BessellianElements(RA3, num, RA2, Dec2, GeocentricDist2, out var X2, out var Y2);
					double num11 = X2 - X;
					double num12 = Y2 - Y;
					double num13 = Math.Sqrt(num11 * num11 + num12 * num12);
					double num14 = X * num11 + Y * num12;
					double value = (X * num12 - Y * num11) / num13;
					double num15 = Math.Floor((0.0 - num14) / num13 / num13 * 24.0);
					if (Math.Abs(value) < 20.0 + ExpandedSearchDistInEarthRadii && num15 > -5.0 && num15 < 29.0)
					{
						string Basis;
						int NumMeasures;
						bool InvalidDiameter;
						double starDiaMAS = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(RA3 * (180.0 / Math.PI) / 15.0, num * (180.0 / Math.PI), magV, magB, magR, out Basis, out NumMeasures, out InvalidDiameter);
						string errorBasis = "Assumed";
						PlanetPrediction(RA3, num, num7, starCatName, magV, magB, magR, 0.1, 0.1, 90.0, errorBasis, num8 - 1.0, num15, ExpandedSearchDistInEarthRadii, out CloseEventFound, Planet, i, UseHorizons, starDiaMAS, starReliability, noGaiaPM, gaiaUCAC4PM, duplicateSource, num9, AutoSaveSearch, OutputFileName);
						if (CloseEventFound)
						{
							break;
						}
					}
				}
			}
			if (ShowProgressBar)
			{
				((Control)CallingForm.pbarSearch).set_Visible(false);
				((Control)CallingForm.cmdCompute).set_Visible(true);
			}
		}

		private static void PlanetPrediction(double RAStar_2000, double DecStar_2000, double PiStar, string StarCatName, double MagV, double MagB, double MagR, double ErrorEllipseMajor, double ErrorEllipseMinor, double ErrorEllipsePA, string ErrorBasis, double EventJD, double H1, double ExpandedSearchDistance, out bool CloseEventFound, int Planet, int MoonNumber, bool UseHorizons, double StarDiaMAS, double SearchExpansionForRadius, bool AutoSaveSearch, string OutputFileName)
		{
			double starReliability = -1.0;
			int duplicateSource = -1;
			int noGaiaPM = -1;
			int gaiaUCAC4PM = -1;
			PlanetPrediction(RAStar_2000, DecStar_2000, PiStar, StarCatName, MagV, MagB, MagR, ErrorEllipseMajor, ErrorEllipseMinor, ErrorEllipsePA, ErrorBasis, EventJD, H1, ExpandedSearchDistance, out CloseEventFound, Planet, MoonNumber, UseHorizons, StarDiaMAS, starReliability, noGaiaPM, gaiaUCAC4PM, duplicateSource, SearchExpansionForRadius, AutoSaveSearch, OutputFileName);
		}

		private static void PlanetPrediction(double RAStar_2000, double DecStar_2000, double PiStar, string StarCatName, double MagV, double MagB, double MagR, double ErrorEllipseMajor, double ErrorEllipseMinor, double ErrorEllipsePA, string ErrorBasis, double EventJD, double H1, double ExpandedSearchDistance, out bool CloseEventFound, int Planet, int MoonNumber, bool UseHorizons, double StarDiaMAS, double StarReliability, int NoGaiaPM, int GaiaUCAC4PM, int DuplicateSource, double SearchExpansionForRadius, bool AutoSaveSearch, string OutputFileName)
		{
			//IL_0e4c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e52: Invalid comparison between Unknown and I4
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDist = 0.0;
			double dRA = 0.0;
			double dDec = 0.0;
			double PAlimb_deg = 0.0;
			double PhaseAngle_deg = 0.0;
			double Planetocentric_Latitude_deg = 0.0;
			double PAPole_deg = 0.0;
			double RA_end = 0.0;
			double Dec_end = 0.0;
			double RA_end2 = 0.0;
			double Dec_end2 = 0.0;
			_ = new double[4];
			_ = new double[4];
			_ = new double[4];
			double num = 0.0;
			double num2 = 0.0;
			double dRA_ComparedToStar = 0.0;
			double dDec_ComparedToStar = 0.0;
			double num3 = 1.0;
			double MoonDiaUncert = 0.1;
			double Uncertainty = 0.0;
			string MoonName = "";
			string EphemSource = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			int num4 = 0;
			bool planetMoonInPlanetShadow = false;
			double X = 0.0;
			double Y = 0.0;
			double num5 = 0.0;
			int Year = 2018;
			int Month = 1;
			double day = 1.0;
			bool TwoEvents = false;
			double[] MidTpair = new double[2];
			double num6 = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			double GeocentricDistance = 0.0;
			bool flag = true;
			bool flag2 = false;
			CloseEventFound = false;
			Application.DoEvents();
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(EventJD + H1 / 24.0, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
				if (Planet == 9)
				{
					Satellites.SatelliteCoordinates(EventJD + H1 / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA -= -0.10860000000000003 * dRA;
					Dec -= -0.10860000000000003 * dDec;
				}
			}
			else
			{
				Satellites.SatelliteCoordinates(EventJD + H1 / 24.0, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				GeocentricDist = Satellites.PlanetDistance;
				if (Planet == 9 && MoonNumber == 1)
				{
					Satellites.SatelliteCoordinates(EventJD + H1 / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA -= -0.10860000000000003 * dRA;
					Dec -= -0.10860000000000003 * dDec;
				}
			}
			BessellianElements(RAStar_2000, DecStar_2000, RA, Dec, GeocentricDist, out var X2, out var Y2);
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(EventJD + (H1 + 1.0) / 24.0, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
				if (Planet == 9)
				{
					Satellites.SatelliteCoordinates(EventJD + (H1 + 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA -= -0.10860000000000003 * dRA;
					Dec -= -0.10860000000000003 * dDec;
				}
			}
			else
			{
				Satellites.SatelliteCoordinates(EventJD + (H1 + 1.0) / 24.0, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				GeocentricDist = Satellites.PlanetDistance;
				if (Planet == 9 && MoonNumber == 1)
				{
					Satellites.SatelliteCoordinates(EventJD + (H1 + 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA -= -0.10860000000000003 * dRA;
					Dec -= -0.10860000000000003 * dDec;
				}
			}
			BessellianElements(RAStar_2000, DecStar_2000, RA, Dec, GeocentricDist, out var X3, out var Y3);
			double dX = X3 - X2;
			double dX2 = Y3 - Y2;
			double n = Math.Sqrt(dX * dX + dX2 * dX2);
			double num7 = X2 * dX + Y2 * dX2;
			double MinimumDist = (X2 * dX2 - Y2 * dX) / n;
			double num8 = Math.Floor(H1 - num7 / n / n);
			if (Math.Abs(MinimumDist) > 4.0 + SearchExpansionForRadius + ExpandedSearchDistance || num8 < -2.0 || num8 > 26.0)
			{
				return;
			}
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(EventJD + num8 / 24.0, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
				if (Planet == 9)
				{
					Satellites.SatelliteCoordinates(EventJD + num8 / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			else
			{
				Satellites.SatelliteCoordinates(EventJD + num8 / 24.0, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				GeocentricDist = Satellites.PlanetDistance;
				if (Planet == 9 && MoonNumber == 1)
				{
					Satellites.SatelliteCoordinates(EventJD + num8 / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			BessellianElements(RAStar_2000, DecStar_2000, RA, Dec, GeocentricDist, out X2, out Y2);
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(EventJD + (num8 + 1.0) / 24.0, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
				if (Planet == 9)
				{
					Satellites.SatelliteCoordinates(EventJD + (num8 + 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			else
			{
				Satellites.SatelliteCoordinates(EventJD + (num8 + 1.0) / 24.0, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				GeocentricDist = Satellites.PlanetDistance;
				if (Planet == 9 && MoonNumber == 1)
				{
					Satellites.SatelliteCoordinates(EventJD + (num8 + 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			BessellianElements(RAStar_2000, DecStar_2000, RA, Dec, GeocentricDist, out X3, out Y3);
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(EventJD + (num8 - 1.0) / 24.0, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
				if (Planet == 9)
				{
					Satellites.SatelliteCoordinates(EventJD + (num8 - 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			else
			{
				Satellites.SatelliteCoordinates(EventJD + (num8 - 1.0) / 24.0, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				GeocentricDist = Satellites.PlanetDistance;
				if (Planet == 9 && MoonNumber == 1)
				{
					Satellites.SatelliteCoordinates(EventJD + (num8 - 1.0) / 24.0, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
					RA += -0.10860000000000003 * dRA;
					Dec += -0.10860000000000003 * dDec;
				}
			}
			BessellianElements(RAStar_2000, DecStar_2000, RA, Dec, GeocentricDist, out var X4, out var Y4);
			Utilities.FitQuadraticTo3Points(X4, X2, X3, out X2, out dX, out var d2X);
			Utilities.FitQuadraticTo3Points(Y4, Y2, Y3, out Y2, out dX2, out var d2X2);
			MinorPlanetOccultationElements.GetTforMinimums(num8, X2, Y2, dX, dX2, d2X, d2X2, ref MidTpair, out TwoEvents, out MinimumDist, out n);
			double num9 = MidTpair[0];
			if (n > 5.0)
			{
				CloseEventFound = true;
			}
			if (Math.Abs(MinimumDist) > 2.0 + SearchExpansionForRadius + ExpandedSearchDistance)
			{
				return;
			}
			CloseEventFound = true;
			if ((!UseHorizons | HorizonsIsDown | !Utilities.InternetIsAvailable()) || MoonNumber == 0)
			{
				Utilities.Relativistic_Differential_Correction(EventJD + num9 / 24.0, RAStar_2000, DecStar_2000, GeocentricDist, out dRA_ComparedToStar, out dDec_ComparedToStar);
				RAStar_2000 -= dRA_ComparedToStar;
				DecStar_2000 -= dDec_ComparedToStar;
				num5 = EventJD + (num9 - 1.0) / 24.0;
				if (MoonNumber == 0)
				{
					Utilities.PlanetGeocentric(num5, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
					if (Planet == 9)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				else
				{
					Satellites.SatelliteCoordinates(num5, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					GeocentricDist = Satellites.PlanetDistance;
					if (Planet == 9 && MoonNumber == 1)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				RA += RACorrection / Math.Cos(DecStar_2000);
				Dec += DecCorrection;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end, out Dec_end);
				BessellianElements(RA_end2, Dec_end2, RA_end, Dec_end, GeocentricDist, out X4, out Y4);
				num5 = EventJD + (num9 + 2.0) / 24.0;
				if (MoonNumber == 0)
				{
					Utilities.PlanetGeocentric(num5, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
					if (Planet == 9)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				else
				{
					Satellites.SatelliteCoordinates(num5, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					GeocentricDist = Satellites.PlanetDistance;
					if (Planet == 9 && MoonNumber == 1)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				RA += RACorrection / Math.Cos(DecStar_2000);
				Dec += DecCorrection;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end, out Dec_end);
				BessellianElements(RA_end2, Dec_end2, RA_end, Dec_end, GeocentricDist, out X, out Y);
				num5 = EventJD + (num9 + 1.0) / 24.0;
				if (MoonNumber == 0)
				{
					Utilities.PlanetGeocentric(num5, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
					if (Planet == 9)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				else
				{
					Satellites.SatelliteCoordinates(num5, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					GeocentricDist = Satellites.PlanetDistance;
					if (Planet == 9 && MoonNumber == 1)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				RA += RACorrection / Math.Cos(DecStar_2000);
				Dec += DecCorrection;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end, out Dec_end);
				BessellianElements(RA_end2, Dec_end2, RA_end, Dec_end, GeocentricDist, out X3, out Y3);
				double num10 = RA_end;
				double num11 = Dec_end;
				num5 = EventJD + num9 / 24.0;
				if (MoonNumber == 0)
				{
					Utilities.PlanetGeocentric(num5, Planet, 0.0, 2, out RA, out Dec, out GeocentricDist);
					if (Planet == 9)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				else
				{
					Satellites.SatelliteCoordinates(num5, Planet, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					GeocentricDist = Satellites.PlanetDistance;
					if (Planet == 9 && MoonNumber == 1)
					{
						Satellites.SatelliteCoordinates(num5, 9, 1, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
						RA += -0.10860000000000003 * dRA;
						Dec += -0.10860000000000003 * dDec;
					}
				}
				RA += RACorrection / Math.Cos(DecStar_2000);
				Dec += DecCorrection;
				Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
				Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, num5, use2006values_Not1976: true, out RA_end, out Dec_end);
				BessellianElements(RA_end2, Dec_end2, RA_end, Dec_end, GeocentricDist, out X2, out Y2);
				num = num10 - RA_end;
				if (Math.Abs(num) > Math.PI)
				{
					num -= Math.PI * 2.0 * (double)Math.Sign(num);
				}
				num = num * 3600.0 * 1.0;
				num2 = (num11 - Dec_end) * 3600.0 * 1.0;
			}
			else
			{
				for (int i = 0; i < 2; i++)
				{
					num5 = EventJD + (num9 - 1.0) / 24.0;
					if (!http.GetHorizonsSatelliteApparentEphemeris(Planet, MoonNumber, num5, UseTTnotUT: true, out var RA3, out var Dec3, out var Delta, out EphemSource, out Uncertainty))
					{
						if ((int)MessageBox.Show("The request to Horizons for an ephemeris for P" + Planet + "M" + MoonNumber.ToString().PadLeft(2, '0') + " has failed\r\n\r\nDo you want to disable Horizons for the remainder of this search?\r\n\r\n[A prediction for the current event will not be generated.]", "Continue using Horizons", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
						{
							HorizonsIsDown = true;
						}
						return;
					}
					Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: true, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
					BessellianElements(RA_end2, Dec_end2, RA3[0], Dec3[0], Delta[0], out X4, out Y4);
					num5 = EventJD + (num9 + 2.0) / 24.0;
					Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: true, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
					BessellianElements(RA_end2, Dec_end2, RA3[3], Dec3[3], Delta[3], out X, out Y);
					num5 = EventJD + (num9 + 1.0) / 24.0;
					Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: true, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
					BessellianElements(RA_end2, Dec_end2, RA3[2], Dec3[2], Delta[2], out X3, out Y3);
					num5 = EventJD + num9 / 24.0;
					Utilities.ApparentStarPosition(RAStar_2000, DecStar_2000, 0.0, 0.0, PiStar, 2000, IncludeRelativisticBending: true, num5, use2006values_Not1976: true, out RA_end2, out Dec_end2);
					BessellianElements(RA_end2, Dec_end2, RA3[1], Dec3[1], Delta[1], out X2, out Y2);
					dX = X3 - X2;
					dX2 = Y3 - Y2;
					n = Math.Sqrt(dX * dX + dX2 * dX2);
					num7 = X2 * dX + Y2 * dX2;
					MinimumDist = (X2 * dX2 - Y2 * dX) / n;
					num9 -= num7 / n / n;
					num = RA3[2] - RA3[1];
					if (Math.Abs(num) > Math.PI)
					{
						num -= Math.PI * 2.0 * (double)Math.Sign(num);
					}
					num = num * 3600.0 * 1.0;
					num2 = (Dec3[2] - Dec3[1]) * 3600.0 * 1.0;
				}
			}
			if (MoonNumber == 0)
			{
				Utilities.PlanetGeocentric(num5, Planet, out GeocentricDist, out var Magnitude, out var _, out PhaseAngle_deg, out var _, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg);
				Mag = (float)Magnitude;
			}
			Utilities.FitCubicTo4_EqualSpaced_Points(X4, X2, X3, X, out X2, out dX, out d2X, out var d3X);
			Utilities.FitCubicTo4_EqualSpaced_Points(Y4, Y2, Y3, Y, out Y2, out dX2, out d2X2, out var d3X2);
			double num12 = dX;
			double num13 = dX2;
			n = Math.Sqrt(num12 * num12 + num13 * num13);
			double num14 = X2;
			double num15 = Y2;
			if (Math.Abs((X2 * dX2 - Y2 * dX) / n) > 1.3 + SearchExpansionForRadius + ExpandedSearchDistance)
			{
				return;
			}
			double num16 = EventJD;
			double num17;
			for (num17 = num9 - Utilities.delta_T(num16) / 3600.0; num17 >= 24.0; num17 -= 24.0)
			{
				num16 += 1.0;
			}
			for (; num17 < 0.0; num17 += 24.0)
			{
				num16 -= 1.0;
			}
			string text = Utilities.Date_from_JD(num16, 0);
			int num18 = (int)Math.Floor(num17);
			Utilities.Date_from_JD(num16, out Year, out Month, out day);
			bool flag3 = false;
			if (Planet == 5 && MoonNumber > 0 && MoonNumber < 5)
			{
				flag3 = true;
			}
			if (Planet == 6 && ((MoonNumber > 2 && MoonNumber < 7) || MoonNumber == 8))
			{
				flag3 = true;
			}
			EventTimes = new double[24];
			EventTypes = new int[24];
			int EventCount;
			double PlanetHeliocentricDistance;
			if (flag3)
			{
				double Longitude = 0.0;
				double Latitude = 0.0;
				Utilities.QuickPlanet(num16 + num17 / 24.0, 3, EquinoxOfDate: true, out var _, out var _, out var _, out var Longitude2, out var _, out PlanetHeliocentricDistance);
				Utilities.RA_DEC_to_Long_Lat(RAStar_2000, DecStar_2000, Utilities.Ecliptic2000_deg_1976 / (180.0 / Math.PI), ref Longitude, ref Latitude);
				double num19 = (Longitude - Longitude2) * (180.0 / Math.PI);
				if (num19 < -180.0)
				{
					num19 += 360.0;
				}
				if (num19 > 180.0)
				{
					num19 -= 360.0;
				}
				Satellites.SatellitePlanetEclipsesTransits(num16, Planet, MoonNumber, Eclipse: true, LocalEventsOnly: false, 0.0, 0.0, ref EventTimes, ref EventTypes, out EventCount);
				if (num19 < 0.0)
				{
					Utilities.Swap(ref EventTimes[0], ref EventTimes[2]);
					Utilities.Swap(ref EventTimes[1], ref EventTimes[3]);
				}
				double num20 = (EventTimes[0] - num16) * 24.0 - num17;
				double num21 = (EventTimes[1] - num16) * 24.0 - num17;
				if (num20 < 0.0 && num21 > 0.0)
				{
					return;
				}
				double num22 = (EventTimes[2] - num16) * 24.0 - num17;
				double num23 = (EventTimes[3] - num16) * 24.0 - num17;
				if (num22 < 0.0 && num23 > 0.0)
				{
					Mag = 16f;
					planetMoonInPlanetShadow = true;
				}
				else
				{
					planetMoonInPlanetShadow = false;
				}
				Satellites.SatellitePlanetEclipsesTransits(num16, Planet, MoonNumber, Eclipse: false, LocalEventsOnly: false, 0.0, 0.0, ref EventTimes, ref EventTypes, out EventCount);
				if (num19 < 0.0)
				{
					Utilities.Swap(ref EventTimes[0], ref EventTimes[2]);
					Utilities.Swap(ref EventTimes[1], ref EventTimes[3]);
				}
				double num24 = (EventTimes[0] - num16) * 24.0 - num17;
				double num25 = (EventTimes[1] - num16) * 24.0 - num17;
				if (num24 < 0.0 && num25 > 0.0)
				{
					return;
				}
			}
			double num26 = (Utilities.SiderealTime_deg(num16, Apparent: true) + 360.98568 * num17 / 24.0) % 360.0;
			double num27;
			for (num27 = 180.0 / Math.PI * RAStar_2000 - num26; num27 > 180.0; num27 -= 360.0)
			{
			}
			for (; num27 < -180.0; num27 += 360.0)
			{
			}
			for (num6 = 180.0 / Math.PI * RA_end2 - num26; num6 > 180.0; num6 -= 360.0)
			{
			}
			for (; num6 < -180.0; num6 += 360.0)
			{
			}
			Utilities.QuickPlanet(num16 + num17 / 24.0, 3, EquinoxOfDate: true, out RA2, out Dec2, out GeocentricDistance);
			double num28;
			for (num28 = RA2 * (180.0 / Math.PI) - num26; num28 > 180.0; num28 -= 360.0)
			{
			}
			for (; num28 < -180.0; num28 += 360.0)
			{
			}
			double num29 = Dec2 * (180.0 / Math.PI);
			string PlanetName;
			if (MoonNumber > 0)
			{
				num3 = Satellites.SatelliteDiameter(Planet, MoonNumber, out MoonDiaUncert);
			}
			else
			{
				PlanetParameters(Planet, out PlanetName, out EventCount, out var _, out var PlanetRadius, out MoonDiaUncert);
				num3 = PlanetRadius * 2.0 * 6378.137;
			}
			if (ApplySiteRestriction)
			{
				flag = true;
				double num30 = 0.0;
				num26 = (Utilities.SiderealTime_deg(num16, Apparent: true) + 360.98568 * num17 / 24.0) % 360.0;
				double num31 = Dec_end2 * (180.0 / Math.PI);
				float z = 0f;
				for (int j = 0; j <= 5; j++)
				{
					Maps.SetGlobeOrientation((num6 - num30 * 15.0) / (180.0 / Math.PI), num31 / (180.0 / Math.PI));
					Maps.GlobeCoords(SiteLongitudeTest / (180.0 / Math.PI), SiteLatitudeTest / (180.0 / Math.PI), out var x, out var y, out z, Mirrored: false);
					X2 = num14 - (double)x + dX * num30 + d2X * num30 * num30 + d3X * num30 * num30 * num30;
					Y2 = num15 - (double)y + dX2 * num30 + d2X2 * num30 * num30 + d3X2 * num30 * num30 * num30;
					double num32 = (0.0 - (X2 * dX + Y2 * dX2)) / n / n;
					num30 += num32;
					if (Math.Abs(num32) < 0.003)
					{
						break;
					}
				}
				double num33 = Math.Abs(X2 * dX2 - Y2 * dX) / n;
				double num34 = Math.Asin(z) * (180.0 / Math.PI);
				Maps.SetGlobeOrientation((num28 - num30 * 15.0) / (180.0 / Math.PI), Dec2);
				Maps.GlobeCoords(SiteLongitudeTest / (180.0 / Math.PI), SiteLatitudeTest / (180.0 / Math.PI), out var _, out var _, out var z2, Mirrored: false);
				double num35 = Math.Asin(z2) * (180.0 / Math.PI);
				if (num34 < -5.0)
				{
					flag = false;
				}
				if (MoonNumber > 0 && num33 > SearchDistance + num3 / 2.0 / 6378.137)
				{
					flag = false;
				}
				if (num35 > 0.0 && MagV > 5.0)
				{
					flag = false;
				}
				if (num35 > -8.0 && MagV > 8.0)
				{
					flag = false;
				}
				if (n < 1.0)
				{
					flag = true;
				}
				if (!flag)
				{
					return;
				}
			}
			double num36 = (num3 + StarDiaMAS / 3600000.0 / (180.0 / Math.PI) * GeocentricDist * 149600000.0) / num3;
			double maxDurn = num3 * num36 / 6378.137 / n * 3600.0;
			num4 = 0;
			if (IncludeWDSSearchInSearch)
			{
				if (SearchWDS | SearchIF)
				{
					num4 = Interferometric_Plus_WDS.StarIsInDoubleCats(RAStar_2000 * (180.0 / Math.PI) / 15.0, DecStar_2000 * (180.0 / Math.PI));
				}
				if (Interferometric_Plus_WDS.IsInAAVSO(RAStar_2000 * (180.0 / Math.PI) / 15.0, DecStar_2000 * (180.0 / Math.PI)).Length > 1)
				{
					num4 += 4;
				}
			}
			if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			flag2 = Kepler2.StarInKepler2(RAStar_2000 * (180.0 / Math.PI), DecStar_2000 * (180.0 / Math.PI));
			OccultationElements occultationElements = new OccultationElements();
			occultationElements.IsAsteroid = false;
			occultationElements.IsPlanet = Planet > 0 && MoonNumber < 1;
			occultationElements.IsPlanetaryMoon = MoonNumber > 0;
			occultationElements.Planet = Planet;
			occultationElements.PlanetaryMoon = MoonNumber;
			occultationElements.ObjectNumber = " P" + Planet + "M" + MoonNumber.ToString().PadLeft(2, '0');
			if (MoonNumber == 0)
			{
				occultationElements.ObjectName = Utilities.Planets[Planet];
			}
			else
			{
				occultationElements.ObjectName = MoonName;
			}
			occultationElements.EventYear = Year;
			occultationElements.EventMonth = Month;
			occultationElements.EventDay = (int)day;
			occultationElements.EventJD = num16;
			occultationElements.MidTime_Hrs = num17;
			occultationElements.XatMin = X2;
			occultationElements.dX = dX;
			occultationElements.d2X = d2X;
			occultationElements.d3X = d3X;
			occultationElements.YatMin = Y2;
			occultationElements.dY = dX2;
			occultationElements.d2Y = d2X2;
			occultationElements.d3Y = d3X2;
			occultationElements.StarCatName = StarCatName;
			occultationElements.RA_Star2000 = RAStar_2000;
			occultationElements.Dec_Star2000 = DecStar_2000;
			occultationElements.RA_Star_Apparent = RA_end2;
			occultationElements.Dec_Star_Apparent = Dec_end2;
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
			PlanetHeliocentricDistance = (occultationElements.MagVAsteroid = (occultationElements.MagRAsteroid = -5.0));
			occultationElements.StarDiamMAS = StarDiaMAS;
			occultationElements.DoubleStarCode = num4;
			occultationElements.IsKepler2Star = flag2;
			occultationElements.MagAsteroid = Mag;
			if (Planet == 9)
			{
				double[] array = new double[6] { 0.0, -2.0, -8.7, -9.2, -11.7, -12.6 };
				occultationElements.MagAsteroid += array[MoonNumber];
			}
			occultationElements.AsteroidDiameter = num3;
			occultationElements.AsteroidDiameterUncertainty = MoonDiaUncert;
			PlanetHeliocentricDistance = (occultationElements.AsteroidShadowDiameter_Penumbral = (occultationElements.AsteroidShadowDiameter_Umbral = num3));
			occultationElements.AsteroidDiameterArcSec = num3 / 725.3 / GeocentricDist;
			double num41 = 0.0;
			double num42 = StarDiaMAS * StarDiaMAS - Math.Pow(num3 / 0.7253 / GeocentricDist, 2.0);
			if (num42 > 0.0)
			{
				num41 = num42 / StarDiaMAS / StarDiaMAS;
			}
			double num43 = -2.5 * Math.Log10(num41 + 1E-09);
			double ExtraMagG = 30.0;
			double ExtraMagR = 30.0;
			int AllStars = 0;
			int BrightStars = 0;
			Gaia.GetNearbyStarsFromCoords(StarCatName, RAStar_2000, DecStar_2000, MagV, Utilities.BesselianYear(num16) - 2000.0, 15.0, out ExtraMagG, out ExtraMagR, out BrightStars, out AllStars, out PlanetName);
			occultationElements.NearbyStars_Bright = BrightStars;
			occultationElements.NearbyStars_All = AllStars;
			occultationElements.NearbyStars_MagAdjusted = ExtraMagG < 20.0 || ExtraMagR < 20.0;
			occultationElements.CombinedMagnitudeV = Utilities.CombinedMagnitude(ExtraMagG, Mag);
			double mag = Utilities.CombinedMagnitude(ExtraMagG, Mag);
			double num44 = Utilities.CombinedMagnitude(mag, MagV);
			double num45 = Utilities.CombinedMagnitude(mag, MagV + num43);
			occultationElements.MagDropV = num45 - num44;
			if (MagR != 0.0 && MagR < 30.0)
			{
				double mag2 = Utilities.CombinedMagnitude(ExtraMagR, (double)Mag - 0.45);
				double num46 = Utilities.CombinedMagnitude(mag2, MagR);
				double num47 = Utilities.CombinedMagnitude(mag2, MagR + num43);
				occultationElements.MagDropR = num47 - num46;
			}
			else
			{
				occultationElements.MagDropR = 0.0;
			}
			occultationElements.MaxDurn = maxDurn;
			occultationElements.DistAsteroid = GeocentricDist;
			occultationElements.hourly_dRA = num * (180.0 / Math.PI) / 15.0;
			occultationElements.hourly_dDec = num2 * (180.0 / Math.PI);
			occultationElements.SubstellarLongitude_J2000_deg = num27;
			occultationElements.SubstellarLatitude_J2000_deg = DecStar_2000 * (180.0 / Math.PI);
			occultationElements.SubstellarLongitude_OfDate_deg = num6;
			occultationElements.SubstellarLatitude_OfDate_deg = Dec_end2 * (180.0 / Math.PI);
			occultationElements.SubSolarLongitude_OfDate_deg = num28;
			occultationElements.SubSolarLatitude_OfDate_deg = num29;
			occultationElements.SolarElongation = Math.Acos(Math.Sin(num29 / (180.0 / Math.PI)) * Math.Sin(DecStar_2000) + Math.Cos(num29 / (180.0 / Math.PI)) * Math.Cos(DecStar_2000) * Math.Cos((num27 - num28) / (180.0 / Math.PI))) * (180.0 / Math.PI);
			occultationElements.ForJWST = false;
			if (MoonNumber == 0)
			{
				occultationElements.PABrightLimb = PAlimb_deg;
				occultationElements.PlanetPhaseAngle = PhaseAngle_deg;
				occultationElements.PlanetocentricEarthDec = Planetocentric_Latitude_deg;
				occultationElements.PAPlanetPole = PAPole_deg;
			}
			occultationElements.PlanetMoonInPlanetShadow = planetMoonInPlanetShadow;
			occultationElements.Error_AsIncreaseInPathWidths = 1.0 + ErrorEllipseMajor / (num3 / 6378.137 * 8.794143836182533 / GeocentricDist);
			occultationElements.ErrorEllipseMajorAxis = ErrorEllipseMajor;
			occultationElements.ErrorEllipseMinorAxis = ErrorEllipseMinor;
			occultationElements.ErrorEllipsePA = ErrorEllipsePA;
			occultationElements.KnownSigma_StarAsterPosns = ErrorEllipseMajor;
			occultationElements.ErrorBasis = ErrorBasis;
			occultationElements.StarReliability = StarReliability;
			occultationElements.NoGaiaPM = NoGaiaPM;
			occultationElements.GaiaUCAC4PM = GaiaUCAC4PM;
			occultationElements.DuplicateSource = DuplicateSource;
			occultationElements.NumAsteroidRings = 0;
			occultationElements.NumAsteroidMoons = 0;
			occultationElements.OrbitSource = Utilities.EphemerisBasis(num16);
			if (MoonNumber > 0)
			{
				if (UseHorizons)
				{
					occultationElements.OrbitSource = occultationElements.OrbitSource + "+JPL#" + EphemSource;
					occultationElements.ErrorEllipseMajorAxis = Uncertainty;
					occultationElements.ErrorEllipseMinorAxis = Uncertainty;
					occultationElements.ErrorEllipsePA = 0.0;
					occultationElements.KnownSigma_StarAsterPosns = Uncertainty;
					occultationElements.Error_AsIncreaseInPathWidths = 1.0 + Uncertainty / (num3 / 6378.137 * 8.794143836182533 / GeocentricDist);
				}
				else
				{
					switch (Planet)
					{
					case 4:
						occultationElements.OrbitSource += "+Sinclair A&A 220 (1989)";
						break;
					case 5:
						if (MoonNumber < 5)
						{
							occultationElements.OrbitSource += "+IMCCE L1 theory";
						}
						else if (MoonNumber == 5)
						{
							occultationElements.OrbitSource += "+http://ssd.jpl.nasa.goc/?sat_elem";
						}
						else
						{
							occultationElements.OrbitSource += "+Notes Scientific et Technical du Bureau des Longitudes S054 (1997)";
						}
						break;
					case 6:
						if (MoonNumber < 9)
						{
							occultationElements.OrbitSource += "+Tass 1.7";
						}
						else if (MoonNumber == 9)
						{
							if (num16 < 2459760.5)
							{
								occultationElements.OrbitSource += "+Desmars et al A&A 553 (2013)";
							}
							else
							{
								occultationElements.OrbitSource += "+Jacobsen A&A Supp 128 (1998) - poor";
							}
						}
						else if (MoonNumber > 11 && MoonNumber < 15)
						{
							occultationElements.OrbitSource += "+Oberti et al A&A 397 (2003)";
						}
						break;
					case 7:
						if (MoonNumber < 6)
						{
							occultationElements.OrbitSource += "+ GUST (Laskar et al A&A 188 (1987)";
						}
						else
						{
							occultationElements.OrbitSource += "+Jacobsen et al Astron J 115 (1998)";
						}
						break;
					case 8:
						if (MoonNumber == 1)
						{
							occultationElements.OrbitSource += "+Emelyanov et al MNRAS 454 (2015)";
						}
						else
						{
							occultationElements.OrbitSource += "+Jacobsen et al A&A 247 (1991)";
						}
						break;
					case 9:
						occultationElements.OrbitSource += "Brozovic et al Icarus (2014)";
						break;
					}
				}
			}
			Utilities.QuickMoon(EventJD + num17 / 24.0, out var RA5, out var Dec5, out var _, out var _, out var _);
			Utilities.Distance(RA5, Dec5, RA2, Dec2, out var Distance2, out PlanetHeliocentricDistance);
			Utilities.Distance(RA5, Dec5, RAStar_2000, DecStar_2000, out var Distance3, out PlanetHeliocentricDistance);
			occultationElements.StarMoonElongation_deg = Distance3 * (180.0 / Math.PI);
			occultationElements.MoonPhase_percent = 50.0 * (1.0 - Math.Cos(Distance2));
			occultationElements.PredictionMJD = Utilities.MJD_now();
			string text2 = StarCatName.Trim().PadLeft(6).Replace(' ', '_');
			int num48 = StarCatName.LastIndexOf("-");
			if ((num48 > 4) & (StarCatName.Length - num48 < 4))
			{
				text2 = text2.Substring(0, num48);
			}
			text2 = text2.PadLeft(20, '0').Substring(16, 4);
			occultationElements.EventID = string.Format("{0,-15}", Year + Month.ToString().PadLeft(2, '0') + day.ToString().PadLeft(2, '0') + occultationElements.ObjectName.Replace(" ", "").Replace("(", "").Replace(")", "")
				.PadRight(6)
				.Substring(0, 3) + text2);
			occultationElements.AsteroidMoons.Clear();
			if (AutoSaveSearch)
			{
				using StreamWriter streamWriter = new StreamWriter(OutputFileName, append: true);
				streamWriter.Write(occultationElements.XML_Elements());
			}
			DisplayMPOccultations.OccElements.Add(occultationElements);
			string text3 = " ";
			if (num4 > 0)
			{
				text3 = "#";
			}
			CallingForm.lstResults.get_Items().Add((object)(StarCatName.PadRight(20) + text3 + " on " + text + "  at " + num18.ToString().PadLeft(2) + "hrs"));
			((Control)CallingForm.lstResults).Refresh();
		}

		private static double PlanetMagLimit(int Planet)
		{
			return Planet switch
			{
				1 => (double)Settings.Default.MagLimitMercury, 
				2 => (double)Settings.Default.MagLimitVenus, 
				4 => (double)Settings.Default.MagLimitMars, 
				5 => (double)Settings.Default.MagLimitJupiter, 
				6 => (double)Settings.Default.MagLimitSaturn, 
				7 => (double)Settings.Default.MagLimitUranus, 
				8 => (double)Settings.Default.MagLimitNeptune, 
				_ => (double)Settings.Default.MagLimitPluto, 
			};
		}
	}
}
