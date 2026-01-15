using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class ExternalCalls
	{
		private const double Radian = 180.0 / Math.PI;

		public static void Set_Path_for_DLL_Data(string ApplicationPath)
		{
			Utilities.Set_Path_for_DLL_Data(ApplicationPath);
		}

		public static void Include_WDS_IF_Search_In_Asteroid_and_Planet_Searches(bool IncludeDoublesSearchInSearch)
		{
			MinorPlanetOccultationElements.IncludeWDSSearchInSearch = IncludeDoublesSearchInSearch;
			PlanetOccultationElements.IncludeWDSSearchInSearch = IncludeDoublesSearchInSearch;
		}

		public static int MinorPlanetSearch_Gaia(string ControlFile)
		{
			//IL_048c: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Unknown result type (might be due to invalid IL or missing references)
			string[] array = new string[21]
			{
				"GaiaCatBaseName", "Start_Y_M_D", "End_Y_M_D", "FirstAsteroidNumber", "LastAsteroidNumber", "MinimumAsteroidDiameter", "AsteroidClass", "LimitingStarMagForSearch", "MinimumDuration", "ExpandedSearchDistInEarthRadii",
				"UseUserElementFile", "UseDefaultErrorSettings", "AppendFlag", "UserElementsFile", "AutoSaveSearch", "OutputFileName", "ShowProgressBar", "ForJWST", "GetNearbyStars", "MagDropLimit",
				"OutputPaths"
			};
			string text = "Gaia12_EDR3";
			string asteroidClass = "";
			string userElementsFile = "";
			string text2 = "GaiaPredictions_External.xml";
			double num = 0.0;
			double num2 = 0.0;
			double result = 10.0;
			double result2 = 12.0;
			double result3 = 2.0;
			double result4 = 0.0;
			double result5 = 0.5;
			int num3 = 0;
			int num4 = 0;
			string text3 = "2023,1,1";
			string text4 = "2023,1,31";
			string text5 = "1";
			string s = "100";
			bool useUserElementFile = false;
			bool useDefaultErrorSettings = false;
			bool appendFlag = false;
			bool flag = true;
			bool flag2 = true;
			bool getNearbyStars = true;
			bool flag3 = false;
			MinorPlanetOccultationElements.SearchWDS = Interferometric_Plus_WDS.CanSearchWDS();
			MinorPlanetOccultationElements.SearchIF = Interferometric_Plus_WDS.CanSearchInterferometric();
			using (StreamReader streamReader = new StreamReader(ControlFile))
			{
				string text6 = "";
				int num5 = array.GetUpperBound(0) + 1;
				int num6 = 0;
				int num7 = 0;
				do
				{
					string text7 = streamReader.ReadLine();
					num6 = text7.IndexOf(">");
					num7 = text7.IndexOf("<", num6);
					if (!(num6 > 0 && num7 > num6 + 1))
					{
						continue;
					}
					text6 = text7.Substring(num6 + 1, num7 - num6 - 1).Trim();
					for (int i = 0; i < num5; i++)
					{
						if (!text7.Contains(array[i]))
						{
							continue;
						}
						switch (i)
						{
						case 0:
							text = text6;
							if (text.Contains("EDR3") && File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text.Replace("EDR3", "DR3")))
							{
								text = text.Replace("EDR3", "DR3");
							}
							break;
						case 1:
							text3 = text6;
							break;
						case 2:
							text4 = text6;
							break;
						case 3:
							text5 = text6;
							break;
						case 4:
							s = text6;
							break;
						case 5:
							double.TryParse(text6, out result);
							break;
						case 6:
							asteroidClass = text6;
							break;
						case 7:
							double.TryParse(text6, out result2);
							break;
						case 8:
							double.TryParse(text6, out result3);
							break;
						case 9:
							double.TryParse(text6, out result4);
							break;
						case 10:
							useUserElementFile = text6.Trim() == "1";
							break;
						case 11:
							useDefaultErrorSettings = text6.Trim() == "1";
							break;
						case 12:
							appendFlag = text6.Trim() == "1";
							break;
						case 13:
							userElementsFile = text6;
							break;
						case 14:
							flag = text6.Trim() == "1";
							break;
						case 15:
							text2 = Utilities.AppPath + "\\Generated files\\" + text6;
							break;
						case 16:
							flag2 = text6.Trim() == "1";
							break;
						case 18:
							getNearbyStars = text6.Trim() == "1";
							break;
						case 19:
							double.TryParse(text6, out result5);
							break;
						case 20:
							flag3 = text6.Trim() == "1";
							break;
						}
						break;
					}
				}
				while (!streamReader.EndOfStream);
			}
			string[] array2 = text3.Split(new char[1] { ',' });
			num = Utilities.JD_from_Date(array2[0], array2[1], array2[2], "0");
			string[] array3 = text4.Split(new char[1] { ',' });
			num2 = Utilities.JD_from_Date(array3[0], array3[1], array3[2], "0");
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin"))
			{
				MessageBox.Show("The specified Gaia file (" + text + ") does not exist", "Date Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return 1;
			}
			if (num2 < num)
			{
				MessageBox.Show("End date is earlier than the start date", "Date Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return 2;
			}
			int result6 = 1;
			num3 = ((!int.TryParse(text5, out result6)) ? Elements.MainAsteroids.GetAsteroidRecord_fromName(text5) : Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result6));
			num4 = ((!int.TryParse(s, out result6)) ? Elements.MainAsteroids.GetAsteroidRecord_fromName(text5) : Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result6));
			if (num2 < num)
			{
				MessageBox.Show("Last asteroid is earlier than the first asteroid", "Date Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return 3;
			}
			flag2 = false;
			if (flag)
			{
				using StreamWriter streamWriter = new StreamWriter(text2);
				streamWriter.WriteLine("<Occultations>");
			}
			MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = result5;
			MinorPlanetOccultationElements.MinorPlanetSearch_Gaia(text, num, num2, num3, num4, result, asteroidClass, result2, result3, 0.0, VarExposure: false, result4, useUserElementFile, useDefaultErrorSettings, appendFlag, userElementsFile, flag, text2, flag2, getNearbyStars, flag3);
			if (flag)
			{
				using StreamWriter streamWriter2 = new StreamWriter(text2, append: true);
				streamWriter2.WriteLine("</Occultations>");
			}
			if (flag3)
			{
				int count = DisplayMPOccultations.OccElements.Count;
				for (int j = 0; j < count; j++)
				{
					DisplayMPOccultations.Set_Data_From_OccultationElements_Record(j, PlotEvent: false);
					string text8 = DisplayMPOccultations.OccElements[j].EventYear + DisplayMPOccultations.OccElements[j].EventMonth.ToString().PadLeft(2, '0') + DisplayMPOccultations.OccElements[j].EventDay.ToString().PadLeft(2, '0') + "d" + DisplayMPOccultations.OccElements[j].EventHour.ToString().PadLeft(2, '0') + "h" + Convert.ToInt32(DisplayMPOccultations.OccElements[j].EventMin).ToString().PadLeft(2, '0') + "m_" + DisplayMPOccultations.OccElements[j].ObjectNumber + DisplayMPOccultations.OccElements[j].ObjectName;
					DisplayMPOccultations.AutoPredictPathCoords(Utilities.AppPath + "\\Generated Files\\" + text8 + "_Path.txt", IncludeSunUp: false);
				}
				DisplayMPOccultations.OccElements.Clear();
			}
			return 0;
		}

		public static void MinorPlanetSearch_UserStars(double StartJD, double EndJD, int FirstAsteroidRecord, int LastAsteroidRecord, double MinimumAsteroidDiameter, double MinimumSemiMajorAxisForAsteroid, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool ForJWST, bool UseUserElementFile, bool UseDefaultErrorSettings, bool AppendFlag, string UserElementsFile, string UserStarCatFile, string OutputFileName, bool UseKnownErrors, double KnownErrorMajor, double KnownErrorMinor, double KnownErrorPA, double KnownErrorSigma, double KnownErrorPathAugmentation)
		{
			Utilities.Set_JPLDE_Availability();
			MinorPlanetOccultationElements.SearchWDS = Interferometric_Plus_WDS.CanSearchWDS();
			MinorPlanetOccultationElements.SearchIF = Interferometric_Plus_WDS.CanSearchInterferometric();
			DisplayMPOccultations.OccElements.Clear();
			MinorPlanetOccultationElements.MinorPlanetSearch_UserStars(StartJD, EndJD, FirstAsteroidRecord, LastAsteroidRecord, MinimumAsteroidDiameter, "", LimitingStarMagForSearch, 0.0, ExpandedSearchDistInEarthRadii, UseUserElementFile, UseDefaultErrorSettings, AppendFlag, UserElementsFile, UserStarCatFile, AutoSaveSearch: false, OutputFileName, ShowProgressBar: false, UseKnownErrors, KnownErrorMajor, KnownErrorMinor, KnownErrorPA, KnownErrorSigma, KnownErrorPathAugmentation);
			using StreamWriter streamWriter = new StreamWriter(OutputFileName);
			streamWriter.WriteLine("<Occultations>");
			for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
			{
				streamWriter.Write(DisplayMPOccultations.OccElements[i].XML_Elements());
			}
			streamWriter.WriteLine("</Occultations>");
		}

		public static void PlanetSearch_UserStar(double StartJD, double EndJD, int Planet, bool IncludeMoons, bool OnlyIncludeLargeMoons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, string UserStarCatFile, string OutputFileName)
		{
			PlanetSearch_UserStar(StartJD, EndJD, Planet, IncludeMoons, OnlyIncludeLargeMoons, UseHorizons: true, LimitingStarMagForSearch, ExpandedSearchDistInEarthRadii, AppendFlag, UserStarCatFile, OutputFileName);
		}

		public static void PlanetSearch_UserStar(double StartJD, double EndJD, int Planet, bool IncludeMoons, bool OnlyIncludeLargeMoons, bool UseHorizons, double LimitingStarMagForSearch, double ExpandedSearchDistInEarthRadii, bool AppendFlag, string UserStarCatFile, string OutputFileName)
		{
			Utilities.Set_JPLDE_Availability();
			PlanetOccultationElements.SearchWDS = Interferometric_Plus_WDS.CanSearchWDS();
			PlanetOccultationElements.SearchIF = Interferometric_Plus_WDS.CanSearchInterferometric();
			DisplayMPOccultations.OccElements.Clear();
			PlanetOccultationElements.PlanetSearch_UserStar(StartJD, EndJD, Planet, IncludeMoons, OnlyIncludeLargeMoons, UseHorizons, LimitingStarMagForSearch, ExpandedSearchDistInEarthRadii, AppendFlag, UserStarCatFile, AutoSaveSearch: false, OutputFileName, ShowProgressBar: false);
			using StreamWriter streamWriter = new StreamWriter(OutputFileName);
			streamWriter.WriteLine("<Occultations>");
			for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
			{
				streamWriter.Write(DisplayMPOccultations.OccElements[i].XML_Elements());
			}
			streamWriter.WriteLine("</Occultations>");
		}

		public static void Auto_ReadElements(string ElementsFileName, int RecordNumber)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.AutoPredictReadElements(ElementsFileName, RecordNumber);
		}

		public static void Auto_WorldGraphic(string OutPutFile_png, bool PlotInColour, string SiteFile, bool AutoScaleMissEvents)
		{
			Utilities.Set_JPLDE_Availability();
			Settings.Default.AsteroidStarChartEnhanced = false;
			DisplayMPOccultations.AutoPredictWorldGraphic(OutPutFile_png, PlotInColour, SiteFile, AutoScaleMissEvents);
		}

		public static void Auto_PathCoordinates(string OutPutFile, bool IncludeSunUp)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.AutoPredictPathCoords(OutPutFile, IncludeSunUp);
		}

		public static void Auto_GoogleEarthKML(string OutPutFile_KML, string KMLLabel, string SiteFile)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.CreateGoogleEarthKMLFile(OutPutFile_KML, KMLLabel, Auto: true, View: false, SiteFile);
		}

		public static void Auto_GoogleMapHTM(string OutPutFile_HTM, string PageName)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.CreateGoogleMapHTMFile(OutPutFile_HTM, PageName, Auto: true);
		}

		public static void Auto_PrePointStars(string OutPutFile)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.AutoPredictPrePointStars(OutPutFile);
		}

		public static void Auto_MultiSite(string OutPutFile, string MultiSiteFileName)
		{
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.AutoPredictMultiSite(MultiSiteFileName, OutPutFile);
		}

		public static void ShowDefaultPage()
		{
			//IL_000a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("You have called OccultUtilities.dll from a program other than Occult.\r\n\r\nUser settings from the Occult environment do not apply when OccultUtilities.dll\r\nis called by a different program. This call allows you to set the various\r\nUser Settings for the environment of the calling program.\r\n\r\nNOTE: it will take a few moments to load the User Settings form.", "External call - User settings");
			((Form)new Defaults()).ShowDialog();
		}

		public static int CreateOutputFiles(string ControlFile)
		{
			double result = 1.0;
			int num = 0;
			bool includeSunUp = false;
			bool flag = false;
			bool flag2 = false;
			Settings.Default.AsteroidStarChartEnhanced = false;
			List<double[]> list = new List<double[]>();
			Utilities.Set_JPLDE_Availability();
			if (MinorPlanetOccultationElements.BinaryElements.Count < 1)
			{
				MinorPlanetOccultationElements.CreateBinaryElementList();
			}
			http.UpdateListOfMiriadeBinaries(MustUpdateNow: false);
			if (MinorPlanetOccultationElements.BinaryAsteroidsInMiriade.Count < 1)
			{
				MinorPlanetOccultationElements.CreateBinaryAstroidsInMiriade();
			}
			string path = Path.GetDirectoryName(ControlFile) + "/" + Path.GetFileNameWithoutExtension(ControlFile) + "-A" + Path.GetExtension(ControlFile);
			MinorPlanetOccultationElements.OffsetRA = (MinorPlanetOccultationElements.OffsetDec = 0.0);
			if (File.Exists(path))
			{
				using StreamReader streamReader = new StreamReader(path);
				do
				{
					string text = streamReader.ReadLine()!.PadRight(15);
					double result2;
					if (text.Substring(0, 11) == "<Offset RA>")
					{
						if (!double.TryParse(text.Substring(11), out result2))
						{
							result2 = 0.0;
						}
						MinorPlanetOccultationElements.OffsetRA = result2 / 3600000.0 / (180.0 / Math.PI);
					}
					else if (text.Substring(0, 12) == "<Offset Dec>")
					{
						if (!double.TryParse(text.Substring(12), out result2))
						{
							result2 = 0.0;
						}
						MinorPlanetOccultationElements.OffsetDec = result2 / 3600000.0 / (180.0 / Math.PI);
					}
				}
				while (!streamReader.EndOfStream);
			}
			string userElementsFile;
			string userStarCatFile;
			string text2;
			string text3;
			double num2;
			double endJD;
			double result4;
			double result5;
			double result6;
			double result7;
			double result8;
			using (StreamReader streamReader2 = new StreamReader(ControlFile))
			{
				userElementsFile = streamReader2.ReadLine();
				userStarCatFile = streamReader2.ReadLine();
				text2 = streamReader2.ReadLine()!.Replace('/', '_');
				_ = text2 + "_Elements.xml";
				text3 = streamReader2.ReadLine();
				if (!double.TryParse(streamReader2.ReadLine(), out var result3))
				{
					return -1;
				}
				num2 = Math.Floor(result3) + 0.5 - 1.0;
				endJD = num2 + 3.0;
				if (!double.TryParse(streamReader2.ReadLine(), out result4))
				{
					result4 = 0.0;
				}
				if (!double.TryParse(streamReader2.ReadLine(), out result5))
				{
					result5 = 0.0;
				}
				if (!double.TryParse(streamReader2.ReadLine(), out result6))
				{
					result6 = 0.0;
				}
				if (!double.TryParse(streamReader2.ReadLine(), out result7))
				{
					result7 = 0.0;
				}
				if (!double.TryParse(streamReader2.ReadLine(), out result8))
				{
					result8 = 0.0;
				}
				if (!double.TryParse(streamReader2.ReadLine(), out result))
				{
					result = 1.0;
				}
				int num3 = int.Parse(streamReader2.ReadLine()!.Trim());
				includeSunUp = (num3 & 1) == 1;
				flag2 = (num3 & 2) == 2;
				double result9;
				double result10;
				while (double.TryParse(streamReader2.ReadLine(), out result9) && double.TryParse(streamReader2.ReadLine(), out result10))
				{
					if (!double.TryParse(streamReader2.ReadLine(), out var result11))
					{
						result11 = 1.0;
					}
					if (result11 < 0.1)
					{
						result11 = 1.0;
					}
					if (!int.TryParse(streamReader2.ReadLine(), out var result12))
					{
						result12 = 0;
					}
					if (!int.TryParse(streamReader2.ReadLine(), out var result13))
					{
						result13 = 0;
					}
					double[] item = new double[5] { result9, result10, result11, result12, result13 };
					list.Add(item);
					if (streamReader2.EndOfStream)
					{
						break;
					}
				}
			}
			DisplayMPOccultations.OccElements.Clear();
			MinorPlanetOccultationElements.MiriadeFirstOnly = !flag2;
			MinorPlanetOccultationElements.MinorPlanetSearch_UserStars(num2, endJD, 0, 0, 0.0, "", 30.0, 0.0, 20.0, UseUserElementFile: true, UseDefaultErrorSettings: false, AppendFlag: false, userElementsFile, userStarCatFile, AutoSaveSearch: false, "", ShowProgressBar: false, UseKnownErrors: true, result4, result5, result6, result7, result8);
			if (DisplayMPOccultations.OccElements.Count < 0)
			{
				return -2;
			}
			string text4 = "";
			int num4 = 1;
			if (MinorPlanetOccultationElements.NumberOfPredictionsForBinarySystem > 1)
			{
				num4 = DisplayMPOccultations.OccElements.Count;
			}
			for (int i = 0; i < num4; i++)
			{
				DisplayMPOccultations.Set_Data_From_OccultationElements_Record(i, PlotEvent: false);
				if (i == 0)
				{
					text4 = "";
				}
				else
				{
					int num5 = DisplayMPOccultations.OccElements[i].EventID.LastIndexOf("_M");
					if (num5 > 0)
					{
						text4 = DisplayMPOccultations.OccElements[i].EventID.Substring(num5);
					}
				}
				using (StreamWriter streamWriter = new StreamWriter(text2 + text4 + "_Elements.xml"))
				{
					streamWriter.WriteLine("<Occultations>");
					streamWriter.Write(DisplayMPOccultations.OccElements[i].XML_Elements());
					streamWriter.WriteLine("</Occultations>");
				}
				using (StreamWriter streamWriter2 = new StreamWriter(text2 + text4 + "_Elements.dat"))
				{
					streamWriter2.Write(DisplayMPOccultations.OccElements[i].ElementsIn540Format);
				}
				DisplayMPOccultations.AutoPredictPathCoords(text2 + text4 + "_Path.txt", includeSunUp);
				if ((text3.Length > 5) & File.Exists(text3))
				{
					DisplayMPOccultations.AutoPredictMultiSite(text3, text2 + " Local times.txt");
				}
				for (int j = 0; j < list.Count; j++)
				{
					double[] item = list[j];
					double result9 = item[0];
					double result10 = item[1];
					double result11 = item[2];
					int result12 = (int)item[3];
					int result13 = (int)item[4];
					flag = result13 == 0;
					DisplayMPOccultations.FilterOnSite = true;
					DisplayMPOccultations.SiteLongitude_Rad = result9 / (180.0 / Math.PI);
					DisplayMPOccultations.SiteLatitude_Rad = result10 / (180.0 / Math.PI);
					DisplayMPOccultations.UseRedrawSites = false;
					switch (result12)
					{
					case 0:
					{
						if (DisplayMPOccultations.GeographicCoordsAtMinD(out var Longitude, out var Latitude))
						{
							DisplayMPOccultations.SiteLongitude_Rad = Longitude / (180.0 / Math.PI);
							DisplayMPOccultations.SiteLatitude_Rad = Latitude / (180.0 / Math.PI);
						}
						break;
					}
					case 1:
					{
						AsteroidPathCoords CalculatedPath = new AsteroidPathCoords();
						bool ValidPath = false;
						DisplayMPOccultations.PathByLongitude(result9, result10, UseLocalTopography: false, 0.0, ref CalculatedPath, out ValidPath);
						if (!ValidPath)
						{
							DisplayMPOccultations.PathByLatitude(result10, result9, UseLocalTopography: false, 0.0, ref CalculatedPath, out ValidPath);
						}
						if (ValidPath)
						{
							DisplayMPOccultations.SiteLongitude_Rad = CalculatedPath.LongitudeCentre / (180.0 / Math.PI);
							DisplayMPOccultations.SiteLatitude_Rad = CalculatedPath.LatitudeCentre / (180.0 / Math.PI);
						}
						break;
					}
					}
					DisplayMPOccultations.PlotMagnification = result11;
					DisplayMPOccultations.AutoPredictWorldGraphic(text2 + j + text4, flag, text3, AutoScaleMissEvents: false);
					num++;
				}
			}
			if (MinorPlanetOccultationElements.MiriadeWasQueried == false)
			{
				return -10;
			}
			if (MinorPlanetOccultationElements.MiriadeWasQueried.GetValueOrDefault())
			{
				return 100 + MinorPlanetOccultationElements.NumberOfPredictionsForBinarySystem;
			}
			return 0;
		}

		public static string GetDoubleStarDetails(double RA, double Dec)
		{
			return Interferometric_Plus_WDS.Find_WDS_IF_Matches(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), HighPrecision: true, ShowResults: false);
		}
	}
}
