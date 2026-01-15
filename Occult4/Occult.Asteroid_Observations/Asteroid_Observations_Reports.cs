using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;
using LightCurves;
using Occult.MPC_PDS;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class Asteroid_Observations_Reports
	{
		public static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static List<string>[] IDforRegions = new List<string>[5];

		internal static DisplayData DataBox;

		internal static DisplayDiameters Display_Diameters;

		internal static DisplayDoubleStars Display_Doubles;

		internal static DisplayRings Display_Rings;

		internal static ListBinaryAsteroid DisplayBinaries;

		internal static ShapeModelFits DisplayShapes;

		internal static DisplayUnfitted Display_Unfitted;

		internal static DisplayNoMainBody Display_NoMainBody;

		private static DisplayPositions Display_Positions;

		private static DisplayErrors Display_Errors;

		private static DisplayLargeUncert Display_LargeUncerts;

		private static DisplayForReview Display_ForReview;

		private static Display_MidTerrors Display_MidT_errors;

		private static DisplayAsteroidsByClass Display_ByClass;

		internal static DisplayEventsByAnObserver DisplayObserver;

		internal static DisplayShapeModels Display_ShapeModels;

		internal static LightCurveData LightCurveData;

		internal static Statistics FindOccultedStars;

		internal static DisplayData DataDisplay;

		internal static PDS_Headers EditPDS_Headers;

		internal static MPCandPDS MPC_PDS;

		internal static Search_FreeText FreeSearch;

		internal static List<AsteroidDiameters> DiameterList = new List<AsteroidDiameters>();

		internal static List<EllipseDiameters> EllipseDiameterList = new List<EllipseDiameters>();

		internal static List<AsteroidDoubleStars> DoublesList = new List<AsteroidDoubleStars>();

		internal static List<BinaryAsteroid> BinaryList = new List<BinaryAsteroid>();

		internal static List<AsteroidsWithShapes> AsteroidsHavingShapes = new List<AsteroidsWithShapes>();

		internal static List<string> DAMITmodelsRemoved = new List<string>();

		internal static List<string> DAMITmodelsUpdated = new List<string>();

		internal static List<ShapeModels> ShapesList = new List<ShapeModels>();

		internal static List<AsteroidPositions> PositionsList = new List<AsteroidPositions>();

		internal static List<DistantAsteroid> DistantList = new List<DistantAsteroid>();

		internal static List<HistoricalIndexData> HistoricalIndex = new List<HistoricalIndexData>();

		internal static List<MPCName> ObserverList = new List<MPCName>();

		internal static List<Asteroid_Observations_StarMatch> StarList = new List<Asteroid_Observations_StarMatch>();

		internal static ArrayList HistoryFiles = new ArrayList();

		internal static List<Observers> ObserverCount = new List<Observers>();

		internal static List<AsteroidNumbers> AsteroidsObserved = new List<AsteroidNumbers>();

		private static List<RegionalStats> stats = new List<RegionalStats>();

		private static List<RegionalStats> Stat
		{
			get
			{
				return stats;
			}
			set
			{
				stats = value;
			}
		}

		public static void CloseAsteroidObservationForms()
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to close all Asteroid Observation forms?", "Close All", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			try
			{
				((Form)Display_Diameters).Close();
				((Component)(object)Display_Diameters).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Display_Doubles).Close();
				((Component)(object)Display_Doubles).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Display_Positions).Close();
				((Component)(object)Display_Positions).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Display_Errors).Close();
				((Component)(object)Display_Errors).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Display_ByClass).Close();
				((Component)(object)Display_ByClass).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)DisplayObserver).Close();
				((Component)(object)DisplayObserver).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.Observations_Editor).Close();
				((Component)(object)Data_and_Plots.Observations_Editor).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.PlotForm).Close();
				((Component)(object)Data_and_Plots.PlotForm).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.TimeBase_Offset).Close();
				((Component)(object)Data_and_Plots.TimeBase_Offset).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.Prediction_Offset).Close();
				((Component)(object)Data_and_Plots.Prediction_Offset).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.Miss_Times).Close();
				((Component)(object)Data_and_Plots.Miss_Times).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.ChordLengths).Close();
				((Component)(object)Data_and_Plots.ChordLengths).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.DiameterFromChords).Close();
				((Component)(object)Data_and_Plots.DiameterFromChords).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.ReductionPlot).Close();
				((Component)(object)Data_and_Plots.ReductionPlot).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)StarDiameterAnalysis.StarDiameter_Analyser).Close();
				((Component)(object)StarDiameterAnalysis.StarDiameter_Analyser).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Display_ShapeModels).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Display_ShapeModels).Close();
			}
			catch
			{
			}
		}

		public static void ListAsteroidDiameters(bool ForPDS)
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetDiameters();
			if (!ForPDS)
			{
				try
				{
					((Control)Display_Diameters).Show();
				}
				catch
				{
					Display_Diameters = new DisplayDiameters();
					((Control)Display_Diameters).Show();
				}
				AsteroidDiameters.SortField = 2;
				DiameterList.Sort();
				Display_Diameters.Display();
				((Control)Display_Diameters).Focus();
			}
		}

		public static void Show_FreeText()
		{
			try
			{
				((Control)FreeSearch).Show();
			}
			catch
			{
				FreeSearch = new Search_FreeText();
				((Control)FreeSearch).Show();
			}
		}

		public static void ShowShapeModels()
		{
			try
			{
				((Control)Display_ShapeModels).Show();
			}
			catch
			{
				Display_ShapeModels = new DisplayShapeModels();
				((Control)Display_ShapeModels).Show();
				((Control)Display_ShapeModels).set_Top(10);
				((Control)Display_ShapeModels).set_Left(50);
			}
			((Control)Display_ShapeModels).Focus();
			Display_ShapeModels.GetModelsForSelectedAsteroid();
		}

		public static void ShowLightCurveData(string AsteroidNumber, string AsteroidName)
		{
			try
			{
				((Control)LightCurveData).Show();
			}
			catch
			{
				LightCurveData = new LightCurveData();
				((Control)LightCurveData).Show();
				((Form)LightCurveData).set_Location(Settings.Default.LocationLightCurveData);
			}
			((Control)LightCurveData).Focus();
			if (!int.TryParse(AsteroidNumber, out var result))
			{
				result = -1;
			}
			if (result > 0)
			{
				((Control)LightCurveData.txtNumber).set_Text(AsteroidNumber);
				((Control)LightCurveData.txtName).set_Text("");
			}
			else
			{
				((Control)LightCurveData.txtNumber).set_Text("");
				((Control)LightCurveData.txtName).set_Text(AsteroidName);
			}
			LightCurveData.Search(ReportErrors: false);
		}

		public static void ListDoublesFromAsteroids()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetDoubles(All: true);
			DoublesList.Sort();
			try
			{
				((Control)Display_Doubles).Show();
			}
			catch
			{
				Display_Doubles = new DisplayDoubleStars();
				((Control)Display_Doubles).Show();
			}
			Display_Doubles.Display();
			((Control)Display_Doubles).Focus();
		}

		public static void ListBinaryAsteroids()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetEventBinaryData();
			BinaryAsteroid.SortField = 2;
			BinaryList.Sort();
			try
			{
				((Control)DisplayBinaries).Show();
			}
			catch
			{
				DisplayBinaries = new ListBinaryAsteroid();
				((Control)DisplayBinaries).Show();
			}
			DisplayBinaries.Display();
			((Control)DisplayBinaries).Focus();
		}

		public static void ListRingedAsteroids()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			string[] array = Data_and_Plots.Historical_AllEvents.GetAsteroidsWithRings().Replace("\r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_Rings).Show();
			}
			catch
			{
				Display_Rings = new DisplayRings();
				((Control)Display_Rings).Show();
			}
			((Control)Display_Rings).Focus();
			Display_Rings.lstRings.get_Items().Clear();
			for (int i = 0; i < array.Length; i++)
			{
				if ((i > 4) & ((i - 2) % 5 == 0))
				{
					Display_Rings.lstRings.get_Items().Add((object)"");
				}
				Display_Rings.lstRings.get_Items().Add((object)array[i]);
			}
		}

		public static void ListShapeModels()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetEventsWithShapeModelsAvailable();
			AsteroidsWithShapes.SortField = 0;
			AsteroidsHavingShapes.Sort();
			try
			{
				((Control)DisplayShapes).Show();
			}
			catch
			{
				DisplayShapes = new ShapeModelFits();
				((Control)DisplayShapes).Show();
			}
			DisplayShapes.ListAsteroids();
			((Control)DisplayShapes).Focus();
		}

		public static void ListDAMITupdatesNeeded()
		{
			//IL_0056: Unknown result type (might be due to invalid IL or missing references)
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetEventsWhereShapeModelIsOutOfDate();
			if ((DAMITmodelsUpdated.Count == 0) & (DAMITmodelsRemoved.Count == 0))
			{
				MessageBox.Show("No editing of DAMIT shape model data is required", "DAMIT good", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			try
			{
				((Control)DataDisplay).Show();
			}
			catch
			{
				DataDisplay = new DisplayData();
				((Control)DataDisplay).Show();
			}
			((Control)DataDisplay).set_Width(600);
			((ToolStripItem)DataDisplay.cmdCancel).set_Visible(false);
			((ToolStripItem)DataDisplay.cmdOK).set_Visible(false);
			string text = "";
			if (DAMITmodelsUpdated.Count > 0)
			{
				text += "Events where shape models have been revised in DAMIT\r\n\r\n";
				for (int i = 0; i < DAMITmodelsUpdated.Count; i++)
				{
					text = text + DAMITmodelsUpdated[i] + "\r\n";
				}
				text += "\r\n";
			}
			if (DAMITmodelsRemoved.Count > 0)
			{
				text += "Events where shape models have been removed from DAMIT\r\n\r\n";
				for (int j = 0; j < DAMITmodelsRemoved.Count; j++)
				{
					text = text + DAMITmodelsRemoved[j] + "\r\n";
				}
			}
			((Control)DataDisplay.txtBox).set_Text(text);
			((TextBoxBase)DataDisplay.txtBox).Select(0, 0);
		}

		public static void ListDiametersFromShapeModelFits(bool ForPDS, string OutPutFile, string PDSHeader, out int DiameterCount)
		{
			string text = "";
			string text2 = "";
			string text3 = "";
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetEventsWithShapeModelsAvailable();
			AsteroidsWithShapes.SortField = 0;
			AsteroidsHavingShapes.Sort();
			ShapeModels.SortField = 1;
			DiameterCount = 0;
			using StreamWriter streamWriter = new StreamWriter(OutPutFile);
			if (ForPDS)
			{
				streamWriter.WriteLine(PDSHeader);
			}
			else
			{
				streamWriter.WriteLine(" Number Name          NEO+AcuA+IRAS diam    Diameter   Evnts  Inv  Shape model 1       Diameter   Evnts  Inv  Shape model 2       Diameter   Evnts  Inv  Shape model 3       Diameter   Evnts  Inv  Shape model 4       Diameter   Evnts  Inv  Shape model 5");
			}
			for (int i = 0; i < AsteroidsHavingShapes.Count; i++)
			{
				string asteroidID = AsteroidsHavingShapes[i].AsteroidID.ToString();
				Data_and_Plots.Historical_AllEvents.GetShapeModels(asteroidID);
				ShapesList.Sort();
				Utilities.Display_IR_AsteroidDiameter(AsteroidsHavingShapes[i].AsteroidNumber, ShowInForm: false, out var Diameters);
				string[] array = Diameters.Split(new char[1] { '\r' });
				string text4 = "".PadRight(12);
				string text5 = "||";
				for (int j = 0; j < array.Length; j++)
				{
					if (array[j].Contains("Weight"))
					{
						if (!ForPDS)
						{
							text4 = array[j].Substring(array[j].IndexOf(":") + 2).Replace("  km", "").PadRight(12);
						}
						else if (array[j].Contains(":"))
						{
							string[] array2 = array[j].Substring(array[j].IndexOf(":") + 2).Replace("km", "").Split(new char[1] { '±' });
							text5 = "|" + array2[0].Trim() + "|" + array2[1].Trim();
						}
						else
						{
							text5 = "||";
						}
						break;
					}
				}
				new List<string>();
				List<double> list = new List<double>();
				bool flag = false;
				string text6 = "0";
				string text7 = "";
				double Mean;
				double SDev;
				for (int k = 0; k < ShapesList.Count; k++)
				{
					if (k == 0)
					{
						text = ShapesList[k].Source.PadRight(6) + ShapesList[k].ID.Trim().PadRight(11);
						text2 = "|" + ShapesList[k].Source + "|" + ShapesList[k].ID.Trim();
						num3 = 1;
						num = 0;
						num2 = 0;
						text3 = (ForPDS ? (ShapesList[k].AsteroidNumber.Trim() + "|" + ShapesList[k].AsteroidID + text5) : (ShapesList[k].AsteroidNumber.ToString().PadLeft(7) + " " + ShapesList[k].AsteroidID.PadRight(16) + "  " + text4));
					}
					else if (ShapesList[k].ID != ShapesList[k - 1].ID)
					{
						if (list.Count > 0)
						{
							Utilities.Mean_Sdev(list, out Mean, out SDev);
							if (Mean > 30.0)
							{
								text6 = "0";
								text7 = "   ";
							}
							else if (Mean > 3.0)
							{
								text6 = "1";
								text7 = " ";
							}
							else
							{
								text6 = "2";
								text7 = "";
							}
							text3 = (ForPDS ? (text3 + string.Format("|{0,1:f" + text6 + "}|{1,1:f" + text6 + "}|{2,1}|{3,1}", Mean, SDev, num, num2) + text2) : (text3 + "|" + (string.Format("{0,1:f" + text6 + "}", Mean) + text7).PadLeft(7) + "±" + (string.Format("{0,1:f" + text6 + "}", SDev) + text7).PadLeft(6) + string.Format("({0,2}) [{1,2}]  ", num, num2) + text));
							num3++;
							flag = true;
						}
						list = new List<double>();
						text = ShapesList[k].Source.PadRight(6) + ShapesList[k].ID.Trim().PadRight(11);
						text2 = "|" + ShapesList[k].Source + "|" + ShapesList[k].ID.Trim();
						num = 0;
						num2 = 0;
					}
					bool flag2 = (ShapesList[k].VolumeDia_Max > 0.0) & (ShapesList[k].VolumeDia_Min > 0.0);
					if (ShapesList[k].FitQuality == 4)
					{
						if (flag2)
						{
							list.Add(ShapesList[k].VolumeDia_Max);
							list.Add(ShapesList[k].VolumeDia_Min);
							num++;
						}
					}
					else if (ShapesList[k].FitQuality == 5)
					{
						if (flag2)
						{
							for (int l = 0; l < 3; l++)
							{
								list.Add(ShapesList[k].VolumeDia_Max);
								list.Add(ShapesList[k].VolumeDia_Min);
								num++;
							}
						}
					}
					else if (ShapesList[k].FitQuality == 6)
					{
						if (flag2)
						{
							for (int m = 0; m < 9; m++)
							{
								list.Add(ShapesList[k].VolumeDia_Max);
								list.Add(ShapesList[k].VolumeDia_Min);
								num++;
							}
						}
					}
					else if (ShapesList[k].FitQuality == 2)
					{
						num2++;
					}
				}
				if (list.Count > 0)
				{
					Utilities.Mean_Sdev(list, out Mean, out SDev);
					if (Mean > 30.0)
					{
						text6 = "0";
						text7 = "   ";
					}
					else if (Mean > 3.0)
					{
						text6 = "1";
						text7 = " ";
					}
					else
					{
						text6 = "2";
						text7 = "";
					}
					text3 = (ForPDS ? (text3 + string.Format("|{0,1:f" + text6 + "}|{1,1:f" + text6 + "}|{2,1}|{3,1}", Mean, SDev, num, num2) + text2) : (text3 + "|" + (string.Format("{0,1:f" + text6 + "}", Mean) + text7).PadLeft(7) + "±" + (string.Format("{0,1:f" + text6 + "}", SDev) + text7).PadLeft(6) + string.Format("({0,2}) [{1,2}]  ", num, num2) + text));
					flag = true;
				}
				else
				{
					num3--;
				}
				if (!flag)
				{
					continue;
				}
				if (!ForPDS)
				{
					streamWriter.Write(text3);
					for (int n = num3; n < 5; n++)
					{
						streamWriter.Write("|".PadRight(43));
					}
					streamWriter.WriteLine("|");
				}
				else
				{
					streamWriter.Write(text3);
					for (int num4 = num3; num4 < 5; num4++)
					{
						streamWriter.Write("||||||");
					}
					streamWriter.WriteLine("");
				}
				DiameterCount++;
			}
		}

		public static void CheckMidTofAllEvents()
		{
			AllEvents allEvents = new AllEvents();
			allEvents.ReadObservationsFile(HistoricalFile: true, "");
			string[] array = allEvents.GetMidTErrors().Replace("\r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_MidT_errors).Show();
			}
			catch
			{
				Display_MidT_errors = new Display_MidTerrors();
				((Control)Display_MidT_errors).Show();
			}
			Display_MidT_errors.lstMidT.get_Items().Clear();
			Display_MidT_errors.lstMidT.get_Items().Add((object)("    " + array.Length + " events with Mid-time of the Event set incorrectly"));
			Display_MidT_errors.lstMidT.get_Items().Add((object)"");
			Display_MidT_errors.lstMidT.get_Items().Add((object)" As entered  Should be Difference   Number Name            Year Mth Day  Star");
			for (int i = 0; i < array.Length; i++)
			{
				if (i % 5 == 0)
				{
					Display_MidT_errors.lstMidT.get_Items().Add((object)"");
				}
				Display_MidT_errors.lstMidT.get_Items().Add((object)array[i]);
			}
		}

		internal static void CreatePositionList(bool WithNames, bool Gaia_DR2_Updates, bool Asteroids, bool Planets, bool AsteroidSatellites, bool ListAll, int YearStart, int YearEnd, double LastJD, int AsteroidNumber, string Observer, bool ObserverOnly, bool ExcludeStarsWithNoPM, bool IncludeExtraData)
		{
			PositionsList.Clear();
			ObserverList.Clear();
			if (YearStart < 2000)
			{
				YearStart = 1700;
			}
			AllEvents allEvents = new AllEvents();
			allEvents.ReadObservationsFile(HistoricalFile: true, "");
			allEvents.GetPositionsAndObservers(WithNames, Gaia_DR2_Updates, Asteroids, Planets, AsteroidSatellites, ListAll, YearStart, YearEnd, LastJD, AsteroidNumber, Observer, ObserverOnly, ExcludeStarsWithNoPM, IncludeExtraData);
			allEvents.OccEvents.Clear();
		}

		public static void ListPositionsFromAsteroids()
		{
			try
			{
				((Control)Display_Positions).Show();
			}
			catch
			{
				Display_Positions = new DisplayPositions();
				((Control)Display_Positions).Show();
			}
			AsteroidPositions.SortField = 1;
			Display_Positions.ReadPositions(LimitToYear: false);
			((Control)Display_Positions).Focus();
		}

		public static void ReindexKepler2()
		{
			if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count == 0)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			PBar pBar = new PBar();
			try
			{
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count);
				((Control)pBar).Show();
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					pBar.pBarFTP.set_Value(i);
					Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i);
					if (Kepler2.StarInKepler2(EventDetails.RA_Star_2000 * (180.0 / Math.PI), EventDetails.Dec_Star_2000 * (180.0 / Math.PI), EventDetails.Year - 2015, out var RecNum) && EventDetails.Kepler2ID < 100)
					{
						EventDetails.Kepler2ID = Kepler2.K2[RecNum].EPIC_ID;
						Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
					}
				}
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			}
			finally
			{
				((IDisposable)pBar)?.Dispose();
			}
		}

		public static void ListDistantAsteroidEvents()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetDistantAsteroids();
			DistantList.Sort();
			try
			{
				((Control)Display_ByClass).Show();
			}
			catch
			{
				Display_ByClass = new DisplayAsteroidsByClass();
				((Control)Display_ByClass).Show();
			}
			Display_ByClass.Display();
			((Control)Display_ByClass).Focus();
		}

		public static void ListKepler2AsteroidEvents()
		{
			if (!File.Exists(AppPath + "\\Resource Files\\Kepler2.dat"))
			{
				return;
			}
			Kepler2.Initialise_Kepler2_ForAsteroids();
			int num = 0;
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			ShowErrors();
			((Control)Display_Errors).set_Width(750);
			((Control)Display_Errors).set_Text("Observations of Kepler2 stars");
			Display_Errors.lstErrors.get_Items().Clear();
			Display_Errors.lstErrors.get_Items().Add((object)"Asteroidal occultations that have involved a Kepler2 star");
			Display_Errors.lstErrors.get_Items().Add((object)"Ast No Name            yyyy  mm dd      star number         EPIC-ID       R.A.        Dec");
			Display_Errors.lstErrors.get_Items().Add((object)"                                                                        h  m   s     °  '   \" ");
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				string eventID = Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: false, Kepler2: true);
				if (eventID.Length > 0)
				{
					Display_Errors.lstErrors.get_Items().Add((object)eventID);
					num++;
					if (num % 5 == 0)
					{
						Display_Errors.lstErrors.get_Items().Add((object)"");
					}
				}
			}
		}

		public static void ShowEventsForObservers()
		{
			try
			{
				((Control)DisplayObserver).Show();
			}
			catch
			{
				DisplayObserver = new DisplayEventsByAnObserver();
				((Control)DisplayObserver).Show();
			}
			((Control)DisplayObserver).Focus();
			DisplayObserver.optName.set_Checked(true);
			((Control)DisplayObserver.txtSearch).Focus();
		}

		internal static void DisplayAstrometricSolution(bool IncludeConjunctionSolution, bool ReDoUpdate)
		{
			if (ReDoUpdate)
			{
				Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			}
			AllEvents.DecimalPlaces_Int_InSolution_Output(EventDetails.Y_Dia);
			_ = 2;
			double num = EventDetails.RefHour_forAnalysis - EventDetails.MidT_forMotions;
			if (num > 12.0)
			{
				num -= 24.0;
			}
			if (num < -12.0)
			{
				num += 24.0;
			}
			double num2 = EventDetails.Parallax + num * EventDetails.dParallax;
			double num3 = Math.Sqrt(EventDetails.dX * EventDetails.dX + EventDetails.dY * EventDetails.dY) / 3600.0;
			double dRA_asec = 0.0;
			double dDec_asec = 0.0;
			double dRA_asec_Uncert = 0.0;
			double dDec_asec_Uncert = 0.0;
			if (EventDetails.GaiaVersion > 1)
			{
				Gaia.Gaia_FrameRotationCorrections(EventDetails.GaiaVersion, EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, EventDetails.MgStar, out dRA_asec, out dDec_asec, out dRA_asec_Uncert, out dDec_asec_Uncert);
			}
			Utilities.Relativistic_Differential_Correction(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 8.794143836182533 / num2, out var dRA_ComparedToStar, out var dDec_ComparedToStar);
			dRA_ComparedToStar *= 648000.0 / Math.PI * Math.Cos(EventDetails.Dec_Star_2000);
			dDec_ComparedToStar *= 648000.0 / Math.PI;
			StringBuilder stringBuilder = new StringBuilder();
			string text = "(" + EventDetails.AsteroidNo + ") " + EventDetails.AsteroidID + " ";
			if (EventDetails.AsteroidNumber.PadRight(5).Substring(0, 1) == "P")
			{
				int.TryParse(EventDetails.AsteroidNumber.PadRight(5).Substring(1, 1), out var result);
				int.TryParse(EventDetails.AsteroidNumber.PadRight(5).Substring(3, 2), out var result2);
				text = Utilities.Planets[result] + " ";
				if (result2 > 0)
				{
					text = text + Utilities.Roman_from_Numeral(result2) + " " + EventDetails.AsteroidID + " ";
				}
			}
			else if (EventDetails.AsteroidNumber.EndsWith("P") | EventDetails.AsteroidNumber.EndsWith("I"))
			{
				text = EventDetails.AsteroidNumber + "/" + EventDetails.AsteroidID + " ";
			}
			else if (EventDetails.AsteroidNumber.Length > 1 && ((EventDetails.AsteroidNumber.Substring(0, 2) == "P/") | (EventDetails.AsteroidNumber.Substring(0, 2) == "C/") | (EventDetails.AsteroidNumber.Substring(0, 2) == "A/")))
			{
				text = EventDetails.AsteroidNumber + " (" + EventDetails.AsteroidID + ") ";
			}
			string text2 = "Center of Figure";
			bool flag = (EventDetails.CentreOfMass_Offset_X != 0.0) | (EventDetails.CentreOfMass_Offset_Y != 0.0);
			if (flag)
			{
				text2 = ((EventDetails.NumberOfSatellites != 0) ? "Barycenter" : "Center of Mass");
			}
			stringBuilder.Append("    Occultation by " + text);
			Utilities.Date_from_JD(EventDetails.JD_EventDate, out var Year, out var Month, out var day);
			stringBuilder.AppendFormat("on {0} {1} {2}", Year, Utilities.ShortMonths[Month], Math.Floor(day));
			stringBuilder.Append("\r\n\r\nThe star position, and the asteroid position, have not been corrected \r\nfor gravitational deflection.");
			stringBuilder.Append("\r\n" + "".PadRight(70, '-'));
			stringBuilder.Append("\r\n    Star details\r\n\r\nIdentifier: " + EventDetails.StarCat + "  " + EventDetails.StarNumber + "\r\n");
			if (EventDetails.Gaia_ID.Length > 1)
			{
				stringBuilder.Append("Gaia id : " + EventDetails.Gaia_ID + "\r\n");
			}
			if (EventDetails.GaiaVersion == 0)
			{
				stringBuilder.Append("Source = Hipparcos\r\n");
			}
			else if (EventDetails.GaiaVersion == 9)
			{
				stringBuilder.Append("Source = Hip + UBSC\r\n");
			}
			else if (EventDetails.GaiaVersion == -1)
			{
				stringBuilder.Append("Source = Other\r\n");
			}
			else
			{
				stringBuilder.Append("Source of position:  Gaia DR" + EventDetails.GaiaVersion + "\r\n");
			}
			string text3 = string.Format("{0,4:f1} mas", EventDetails.StarDia_mas);
			if (EventDetails.GaiaVersion < 2)
			{
				text3 = "n/a";
			}
			stringBuilder.Append("Estimated diameter [Gaia] " + text3);
			int num4 = EventDetails.IssuesFlag & 1;
			int num5 = EventDetails.IssuesFlag & 2;
			stringBuilder.Append("\r\nConcerns:  RUWE - ");
			if (num4 == 0)
			{
				stringBuilder.Append("None");
			}
			else
			{
				stringBuilder.Append("Yes ");
			}
			stringBuilder.Append("    Duplicate source - ");
			if (num5 == 0)
			{
				stringBuilder.Append("None\r\n");
			}
			else
			{
				stringBuilder.Append("Yes \r\n");
			}
			stringBuilder.AppendFormat("\r\nMagnitudes:   Mb {0,5:f2}    Mg {1,5:f2}    Mr {2,5:f2}\r\n", EventDetails.MbStar, EventDetails.MgStar, EventDetails.MrStar);
			double num6 = EventDetails.RA_Offset_DoubleStar_mas / 1000.0 / Math.Cos(EventDetails.Dec_Star_2000) / 3600.0;
			double num7 = EventDetails.Dec_Offset_DoubleStar_mas / 1000.0 / 3600.0;
			stringBuilder.AppendFormat("\r\nGCRS position at {0,8:f4}", Utilities.BesselianYear(EventDetails.JD_EventDate));
			if (EventDetails.GaiaVersion > 1)
			{
				string text4 = "EDR3";
				if (EventDetails.GaiaVersion == 2)
				{
					text4 = "DR2";
				}
				stringBuilder.Append(" + " + text4 + " frame rotation corrn");
			}
			stringBuilder.Append("\r\n  RA  " + Utilities.DEGtoDMS((EventDetails.RA_Star_2000 * (180.0 / Math.PI) + num6 + dRA_asec / Math.Cos(EventDetails.Dec_Star_2000) / 3600.0) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true) + "    Dec " + Utilities.DEGtoDMS(EventDetails.Dec_Star_2000 * (180.0 / Math.PI) + num7 + dDec_asec / 3600.0, 3, 5, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true) + "\r\n");
			if (num6 != 0.0 || num7 != 0.0)
			{
				stringBuilder.AppendFormat("\r\nDouble star with {0,1:f0} solutions. GCRS position includes the offset to", EventDetails.NumberOfDoubleSolutions);
				stringBuilder.AppendFormat("\r\nthe component used for the solution:  dRA = {0,1:f2} mas, dDec= {1,1:f2} mas\r\n", EventDetails.RA_Offset_DoubleStar_mas, EventDetails.Dec_Offset_DoubleStar_mas);
			}
			stringBuilder.Append("\r\nUncertainties");
			stringBuilder.AppendFormat("\r\n         Star catalogue position: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", EventDetails.RA_Star_Uncertainty_mas, EventDetails.Dec_Star_Uncertainty_mas);
			if ((EventDetails.RA_Offset_DoubleStar_sDev_mas != 0.0) | (EventDetails.Dec_Offset_DoubleStar_sDev_mas != 0.0))
			{
				stringBuilder.AppendFormat("\r\n  from multiple double solutions: RA  ± {0,1:f2} mas, Dec  ± {1,1:f2} mas\r\n", EventDetails.RA_Offset_DoubleStar_sDev_mas, EventDetails.Dec_Offset_DoubleStar_sDev_mas);
			}
			if ((EventDetails.GaiaVersion == 2) & (EventDetails.MgStar < 12.5))
			{
				stringBuilder.AppendFormat("\r\n      DR2 frame rotation: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", dRA_asec_Uncert * 1000.0, dDec_asec_Uncert * 1000.0);
			}
			if (global::GaiaDoubles.GaiaDoubles.GaiaDouble_Match(EventDetails.RA_Star_2000 * (180.0 / Math.PI), EventDetails.Dec_Star_2000 * (180.0 / Math.PI), GaiaCatIDOnly: false, DetailsOnly: true, out var FullDetails))
			{
				stringBuilder.Append("\r\nGaia Non-Single-Star entry\r\n" + FullDetails + "\r\n");
			}
			stringBuilder.Append("\r\n" + "".PadRight(70, '='));
			stringBuilder.Append("\r\n    " + text + "position at event time\r\n");
			string text5 = string.Format("{0,4:F0} {1} {2:00.00000000}", EventDetails.Year, Utilities.ShortMonths[EventDetails.Month], (double)EventDetails.Day + EventDetails.RefHour_forAnalysis / 24.0);
			double num8 = Utilities.JD_from_Date(EventDetails.Year, EventDetails.Month, (double)EventDetails.Day + EventDetails.RefHour_forAnalysis / 24.0);
			Utilities.Date_from_JD(num8, out var Year2, out var Month2, out var day2);
			int num9 = (int)Math.Floor(day2);
			double degree = (day2 - (double)num9) * 24.0;
			stringBuilder.Append("\r\nEvent time :  " + text5 + string.Format("  ±{0,1:f8}", EventDetails.RefHour_forAnalysis_Uncert_secs / 86400.0));
			stringBuilder.Append("\r\n           =  " + Year2 + " " + Utilities.ShortMonths[Month2] + " " + num9.ToString().PadLeft(2, '0') + " " + Utilities.DEGtoDMS(degree, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true).Insert(5, "m").Insert(2, "h") + "s" + string.Format("  ±{0,1:f3}s", EventDetails.RefHour_forAnalysis_Uncert_secs));
			stringBuilder.Append("\r\n                       " + string.Format("(with Across-path uncertainty = ±{0,1:f2} mas)", EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas));
			if (EventDetails.UnseenBinaryPrimary)
			{
				stringBuilder.Append("\r\nNo occultation of the primary body was observed.\r\nThe following position is for a 'predicted' event\r\n");
			}
			stringBuilder.AppendFormat("\r\n\r\nObserved position on the Fundamental Plane:");
			if (flag)
			{
				stringBuilder.AppendFormat("\r\n  Center of Mass   :  X =  {0,1:f3} km   Y =  {1,1:f3} km", EventDetails.X_Geo_CenterOfMass_atEvent, EventDetails.Y_Geo_CenterOfMass_atEvent);
			}
			stringBuilder.AppendFormat("\r\n  Center of Figure :  X =  {0,1:f3} km   Y =  {1,1:f3} km", EventDetails.X_Geo_atEvent, EventDetails.Y_Geo_atEvent);
			stringBuilder.AppendFormat("\r\n  Number of chords :  {0:#}\r\n", EventDetails.Number_Chords);
			double degree2 = (EventDetails.RA_Star_2000 * (180.0 / Math.PI) + num6 + (dRA_asec + EventDetails.dRACosDec_atEvent) / 3600.0 / Math.Cos(EventDetails.Dec_Star_2000)) / 15.0;
			double degree3 = EventDetails.Dec_Star_2000 * (180.0 / Math.PI) + num7 + (dDec_asec + EventDetails.dDec_atEvent) / 3600.0;
			double EquatorialJ2000_X;
			double EquatorialJ2000_Y;
			double EquatorialJ2000_Z;
			if (flag)
			{
				stringBuilder.Append("\r\n".PadRight(20, '-'));
				stringBuilder.Append("\r\n" + text2);
				Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, use2006values_Not1976: false, num8, EventDetails.X_Geo_CenterOfMass_atEvent, EventDetails.Y_Geo_CenterOfMass_atEvent, out EquatorialJ2000_X, out EquatorialJ2000_Y, out EquatorialJ2000_Z);
				stringBuilder.Append("\r\n\r\n Equatorial x,y,z coords on Fundamental Plane");
				stringBuilder.AppendFormat("\r\n X =  {0,1:f3} km   Y =  {1,1:f3} km   Z =  {2,1:f3} km", EquatorialJ2000_X, EquatorialJ2000_Y, EquatorialJ2000_Z);
				stringBuilder.Append("\r\n\r\nGeocentric position");
				stringBuilder.AppendFormat("\r\n   Observed offset from the star - Apparent position:\r\n     dRA =  {0:+0.00000;-0.00000}\"   dDec =  {1:+0.00000;-0.00000}\" \r\n", EventDetails.X_Geo_CenterOfMass_atEvent / 6378.137 * num2, EventDetails.Y_Geo_CenterOfMass_atEvent / 6378.137 * num2);
				stringBuilder.AppendFormat("-> Observed offset from the star - J2000 position:\r\n     dRA =  {0:+0.00000;-0.00000}\"   dDec =  {1:+0.00000;-0.00000}\"\r\n", EventDetails.dRACosDec_atEvent, EventDetails.dDec_atEvent);
				stringBuilder.Append("-> " + text + "J2000 GCRS " + text2 + " at Event time:");
				stringBuilder.Append("\r\n      RA = " + Utilities.DEGtoDMS(degree2, 3, 6, MinutesOnly: false) + "   Dec = " + Utilities.DEGtoDMS(degree3, 3, 5, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
			}
			if (!flag | (EventDetails.NumberOfSatellites > 0))
			{
				stringBuilder.Append("\r\n".PadRight(20, '-'));
				if (EventDetails.NumberOfSatellites == 0)
				{
					stringBuilder.Append("\r\nCenter of Figure");
				}
				else
				{
					stringBuilder.Append("\r\nCenter of Figure of primary component");
				}
				Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, use2006values_Not1976: false, num8, EventDetails.X_Geo_atEvent, EventDetails.Y_Geo_atEvent, out EquatorialJ2000_X, out EquatorialJ2000_Y, out EquatorialJ2000_Z);
				stringBuilder.Append("\r\n\r\n Equatorial x,y,z coords - Center of Figure on Fundamental Plane");
				stringBuilder.AppendFormat("\r\n X =  {0,1:f3} km   Y =  {1,1:f3} km   Z =  {2,1:f3} km", EquatorialJ2000_X, EquatorialJ2000_Y, EquatorialJ2000_Z);
				stringBuilder.Append("\r\n\r\nGeocentric solution for Centre of Figure position");
				stringBuilder.AppendFormat("\r\n   Observed offset from the star - Apparent position:\r\n     dRA =  {0:+0.00000;-0.00000}\"   dDec =  {1:+0.00000;-0.00000}\" \r\n", EventDetails.X_Geo_atEvent / 6378.137 * num2, EventDetails.Y_Geo_atEvent / 6378.137 * num2);
				Data_and_Plots.BessellianElementsInReverse(EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent, num2, EventDetails.X_Geo_atEvent / 6378.137, EventDetails.Y_Geo_atEvent / 6378.137, out var dRA_Asteroid_asec, out var dDec_Asteroid_asec);
				Utilities.ApparentOffsetToJ2000Offset(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, dRA_Asteroid_asec, dDec_Asteroid_asec, out var J2000OffsetRA_asec, out var J2000OffsetDec_asec);
				stringBuilder.AppendFormat("-> Observed offset from the star - J2000 position:\r\n     dRA =  {0:+0.00000;-0.00000}\"   dDec =  {1:+0.00000;-0.00000}\"\r\n", J2000OffsetRA_asec, J2000OffsetDec_asec);
				stringBuilder.Append("-> " + text + "J2000 GCRS " + text2 + " at Event time:");
				double degree4 = (EventDetails.RA_Star_2000 * (180.0 / Math.PI) + num6 + (dRA_asec + J2000OffsetRA_asec) / 3600.0 / Math.Cos(EventDetails.Dec_Star_2000)) / 15.0;
				double degree5 = EventDetails.Dec_Star_2000 * (180.0 / Math.PI) + num7 + (dDec_asec + J2000OffsetDec_asec) / 3600.0;
				stringBuilder.Append("\r\n      RA = " + Utilities.DEGtoDMS(degree4, 3, 6, MinutesOnly: false) + "   Dec = " + Utilities.DEGtoDMS(degree5, 3, 5, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
			}
			stringBuilder.Append("\r\n\r\n".PadRight(20, '-'));
			stringBuilder.Append("\r\nUncertainties    (including time uncertainty)");
			CalculateUncertainties(EventDetails.RA_Star_Uncertainty_mas, EventDetails.Dec_Star_Uncertainty_mas, 0.0, 0.0, 0.0, 0.0, EventDetails.RefHour_forAnalysis_Uncert_secs, EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas, num3 * num2 * 1000.0, EventDetails.Sdev_AlongTrack, EventDetails.Sdev_Sep_Conj, EventDetails.PA_Conj_2000 + 90.0, out var rmsRA_Total_mas, out var rmsDec_Total_mas, out var _, out var _, out var _, out var _);
			stringBuilder.AppendFormat("\r\n             Star position: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", EventDetails.RA_Star_Uncertainty_mas, EventDetails.Dec_Star_Uncertainty_mas);
			if ((EventDetails.RA_Offset_DoubleStar_sDev_mas != 0.0) | (EventDetails.Dec_Offset_DoubleStar_sDev_mas != 0.0))
			{
				stringBuilder.AppendFormat("\r\n multiple double solutions: RA  ± {0,1:f2} mas, Dec  ± {1,1:f2} mas\r\n", EventDetails.RA_Offset_DoubleStar_sDev_mas, EventDetails.Dec_Offset_DoubleStar_sDev_mas);
			}
			if ((EventDetails.GaiaVersion == 2) & (EventDetails.MgStar < 12.5))
			{
				stringBuilder.AppendFormat("\r\n       DR2 frame rotation: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", dRA_asec_Uncert * 1000.0, dDec_asec_Uncert * 1000.0);
			}
			stringBuilder.AppendFormat("\r\n" + text.PadLeft(23) + "fit: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", rmsRA_Total_mas, rmsDec_Total_mas);
			double num10 = Math.Sqrt(Math.Pow(EventDetails.RA_Star_Uncertainty_mas, 2.0) + Math.Pow(EventDetails.RA_Offset_DoubleStar_sDev_mas, 2.0) + Math.Pow(dRA_asec_Uncert * 1000.0, 2.0) + Math.Pow(rmsRA_Total_mas, 2.0));
			double num11 = Math.Sqrt(Math.Pow(EventDetails.Dec_Star_Uncertainty_mas, 2.0) + Math.Pow(EventDetails.Dec_Offset_DoubleStar_sDev_mas, 2.0) + Math.Pow(dDec_asec_Uncert * 1000.0, 2.0) + Math.Pow(rmsDec_Total_mas, 2.0));
			stringBuilder.AppendFormat("\r\n       Total uncertainties: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", num10, num11);
			stringBuilder.AppendFormat("\r\n                          = RA  ±{0,8:f6}s, Dec  ±{1,7:f5}\"", num10 / 15000.0 / Math.Cos(EventDetails.Dec_Star_2000), num11 / 1000.0);
			stringBuilder.Append("\r\n" + "".PadRight(70, '='));
			double num12 = 0.0;
			double num13 = 0.0;
			double num14 = 0.0;
			double num15 = 0.0;
			double num16 = 0.0;
			double num17 = 0.0;
			double num18 = 0.0;
			double num19 = 0.0;
			int num20 = 0;
			if (IncludeConjunctionSolution)
			{
				num12 = EventDetails.Sep_Conj * Math.Cos(EventDetails.PA_Conj_2000 / (180.0 / Math.PI)) / 3600.0;
				num13 = EventDetails.Sep_Conj * Math.Sin(EventDetails.PA_Conj_2000 / (180.0 / Math.PI)) / 3600.0;
				num13 /= Math.Cos(EventDetails.Dec_Star_2000 + num12 / (180.0 / Math.PI) / 2.0);
				num14 = (EventDetails.RA_Star_2000 * (180.0 / Math.PI) + num6 * (180.0 / Math.PI) + dRA_asec / 3600.0 / Math.Cos(EventDetails.Dec_Star_2000) + num13) / 15.0;
				num15 = EventDetails.Dec_Star_2000 * (180.0 / Math.PI) + num7 * (180.0 / Math.PI) + dDec_asec / 3600.0 + num12;
				num20 = (int)Math.Floor(EventDetails.Day_Conj);
				stringBuilder.Append("\r\n" + "".PadRight(70, '-'));
				stringBuilder.Append("\r\n    Asteroid position at Geocentric conjunction\r\n");
				stringBuilder.Append("\r\nConjunction time : " + EventDetails.Year_Conj + " " + Utilities.ShortMonths[EventDetails.Month_Conj] + " " + $"{EventDetails.Day_Conj:00.0000000}");
				stringBuilder.Append("\r\n                 = " + EventDetails.Year_Conj + " " + Utilities.ShortMonths[EventDetails.Month_Conj] + " " + num20.ToString().PadLeft(2, '0') + " " + Utilities.DEGtoDMS(24.0 * (EventDetails.Day_Conj - (double)num20), 2, 2, MinutesOnly: false, IncludeLeadingZeros: true).Insert(5, "m").Insert(2, "h") + "s");
				stringBuilder.Append(string.Format("\r\nAt conjunction\r\n  Separation  {0,7:f4} ±{1,5:f4} asec,  PA {2,7:f3}°", EventDetails.Sep_Conj, EventDetails.Sdev_Sep_Conj / 1000.0, EventDetails.PA_Conj_2000));
				stringBuilder.Append("\r\n\r\n-> Asteroid J2000 GCRS position at Conjunction:");
				stringBuilder.Append("\r\n   RA = " + Utilities.DEGtoDMS(num14, 2, 6, MinutesOnly: false) + "   Dec = " + Utilities.DEGtoDMS(num15, 3, 5, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
				stringBuilder.Append("\r\n\r\nUncertainties");
				stringBuilder.AppendFormat("\r\n           Star position: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", EventDetails.RA_Star_Uncertainty_mas, EventDetails.Dec_Star_Uncertainty_mas);
				if ((EventDetails.GaiaVersion == 2) & (EventDetails.MgStar < 12.5))
				{
					stringBuilder.AppendFormat("\r\n      DR2 frame rotation: RA  ±{0,5:f2} mas, Dec  ±{1,5:f2} mas", dRA_asec_Uncert * 1000.0, dDec_asec_Uncert * 1000.0);
				}
				for (num16 = EventDetails.PA_Conj_2000 + 90.0; num16 > 180.0; num16 -= 180.0)
				{
				}
				for (; num16 < 0.0; num16 += 180.0)
				{
				}
				for (num17 = EventDetails.PA_Conj_2000; num17 > 180.0; num17 -= 180.0)
				{
				}
				for (; num17 < 0.0; num17 += 180.0)
				{
				}
				stringBuilder.AppendFormat("\r\n\r\n    Time of conjunction : ");
				stringBuilder.Append("\r\nrecording uncertainties : " + string.Format("± {0,9:f7} dy", EventDetails.RefHour_forAnalysis_Uncert_secs / 86400.0) + string.Format("  = ± {0,4:f2} secs", EventDetails.RefHour_forAnalysis_Uncert_secs));
				stringBuilder.Append("\r\n      from Asteroid fit : " + string.Format("± {0,9:f7} dy", EventDetails.Sdev_T_Conj) + string.Format("  = ± {0,4:f2} secs", EventDetails.Sdev_T_Conj * 24.0 * 3600.0));
				stringBuilder.Append("\r\n                  Total : " + string.Format("± {0,9:f7} dy", Math.Sqrt(Math.Pow(EventDetails.RefHour_forAnalysis_Uncert_secs / 86400.0, 2.0) + Math.Pow(EventDetails.Sdev_T_Conj, 2.0))) + string.Format("  = ± {0,4:f2} secs", Math.Sqrt(Math.Pow(EventDetails.RefHour_forAnalysis_Uncert_secs, 2.0) + Math.Pow(EventDetails.Sdev_T_Conj * 86400.0, 2.0))));
				num18 = num3 * EventDetails.RefHour_forAnalysis_Uncert_secs * num2 * 1000.0;
				num19 = Math.Sqrt(Math.Pow(EventDetails.Sdev_AlongTrack, 2.0) + Math.Pow(num18, 2.0));
				stringBuilder.Append(string.Format("\r\n                       => ± {0,2:f2} mas along track (in PA {1,7:f3}°)", num19, num16));
				stringBuilder.Append(string.Format("\r\nCross-track uncertainty : ± {0,2:f2} mas (in PA {1,7:f3}°)", EventDetails.Sdev_Sep_Conj, num17));
			}
			if (EventDetails.NumberOfSatellites > 0)
			{
				for (int i = 0; i < EventDetails.NumberOfSatellites; i++)
				{
					stringBuilder.Append("\r\n    Satellite :  " + EventDetails.Satellites[i].CompanionIAUname);
					stringBuilder.Append("\r\n" + "".PadRight(70, '-'));
					stringBuilder.Append("\r\n    Satellite position at event time");
					stringBuilder.Append("\r\n\r\nEvent time :  " + text5);
					stringBuilder.Append("\r\n           =  " + EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day.ToString().PadLeft(2, '0') + " " + Utilities.DEGtoDMS(EventDetails.RefHour_forAnalysis, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true).Insert(5, "m").Insert(2, "h") + "s");
					stringBuilder.Append("\r\n\r\nSeparation and position angle - from Primary body");
					stringBuilder.AppendFormat("\r\n  Apparent: Separation {0,5:f4}\" ±{1,2:f1} mas,  PA {2,2:f3}° ±{3,5:f2}°", EventDetails.Satellites[i].SatelliteSeparation / 1000.0, EventDetails.Satellites[i].Sat_Sep_Uncertainty, EventDetails.Satellites[i].SatellitePA_Apparent, EventDetails.Satellites[i].Sat_PA_Uncertainty);
					stringBuilder.AppendFormat("\r\n  J2000:    Separation {0,5:f4}\" ±{1,2:f1} mas,  PA {2,2:f3}° ±{3,5:f2}°", EventDetails.Satellites[i].SatelliteSeparation / 1000.0, EventDetails.Satellites[i].Sat_Sep_Uncertainty, EventDetails.Satellites[i].SatellitePA_2000, EventDetails.Satellites[i].Sat_PA_Uncertainty);
					if (EventDetails.UnseenBinaryPrimary)
					{
						stringBuilder.Append("\r\nSeparation and PA are from the 'predicted' main body location");
					}
					stringBuilder.AppendFormat("\r\n\r\n-> Offset from the main body  :  dX =  {0,1:f3} km    dY =  {1,1:f3} km", EventDetails.Satellites[i].XSat_Geo_atEvent - EventDetails.X_Geo_atEvent, EventDetails.Satellites[i].YSat_Geo_atEvent - EventDetails.Y_Geo_atEvent);
					stringBuilder.AppendFormat("\r\n-> coords on Fundamental Plane:   X =  {0,1:f3} km    Y =  {1,1:f3} km\r\n", EventDetails.Satellites[i].XSat_Geo_atEvent, EventDetails.Satellites[i].YSat_Geo_atEvent);
					stringBuilder.AppendFormat("\r\n  Number of chords :  {0:#}\r\n", EventDetails.Satellites[i].NumberOfChords);
					Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, use2006values_Not1976: false, num8, EventDetails.Satellites[i].XSat_Geo_atEvent, EventDetails.Satellites[i].YSat_Geo_atEvent, out EquatorialJ2000_X, out EquatorialJ2000_Y, out EquatorialJ2000_Z);
					stringBuilder.Append("\r\n Equatorial x,y,z coords on Fundamental Plane");
					stringBuilder.AppendFormat("\r\n X =  {0,1:f3} km   Y =  {1,1:f3} km   Z =  {2,1:f3} km", EquatorialJ2000_X, EquatorialJ2000_Y, EquatorialJ2000_Z);
				}
			}
			try
			{
				((Control)DataBox).Show();
			}
			catch
			{
				DataBox = new DisplayData();
				((Control)DataBox).Show();
				((Control)DataBox).set_Width(580);
				((Control)DataBox).set_Height(1000);
			}
			((ToolStripItem)DataBox.cmdCancel).set_Visible(false);
			((ToolStripItem)DataBox.cmdOK).set_Visible(false);
			((Control)DataBox).set_Text("Astrometric solution");
			((Control)DataBox.txtBox).set_Text(stringBuilder.ToString());
			((TextBoxBase)DataBox.txtBox).Select(0, 0);
			((Control)DataBox).Focus();
		}

		internal static void CalculateUncertainties(double rmsRAstar_mas, double rmsDecStar_mas, double rmsRADoubleOffset_mas, double rmsDecDoubleOffset_mas, double rmsRAFrameRotate_mas, double rmsDecFrameRotate_mas, double rmsTimeSecs, double rmsAcrossPath_fromTime_mas, double Motion_masSec, double rmsAlongTrack_fromFit_mas, double rmsAcrossTrack_fromFit_mas, double PA_AlongTrack_2000, out double rmsRA_Total_mas, out double rmsDec_Total_mas, out double Correlation, out double rmsStarRA_Total_mas, out double rmsStarDec_Total_mas, out double rmsAlongTrack_Total_mas)
		{
			double num = (90.0 - PA_AlongTrack_2000) / (180.0 / Math.PI);
			rmsAlongTrack_Total_mas = Utilities.QuadratureAddition(rmsAlongTrack_fromFit_mas, rmsTimeSecs * Motion_masSec);
			double num2 = Utilities.QuadratureAddition(rmsAcrossTrack_fromFit_mas, rmsAcrossPath_fromTime_mas);
			if (rmsAlongTrack_Total_mas == 0.0)
			{
				rmsAlongTrack_Total_mas = 0.1;
			}
			if (num2 == 0.0)
			{
				num2 = 0.1;
			}
			double num3 = Math.Pow(rmsAlongTrack_Total_mas * Math.Cos(num), 2.0) + Math.Pow(num2 * Math.Sin(num), 2.0);
			double num4 = (rmsAlongTrack_Total_mas * rmsAlongTrack_Total_mas - num2 * num2) * Math.Cos(num) * Math.Sin(num);
			double num5 = Math.Pow(rmsAlongTrack_Total_mas * Math.Sin(num), 2.0) + Math.Pow(num2 * Math.Cos(num), 2.0);
			rmsRA_Total_mas = Math.Sqrt(num3);
			rmsDec_Total_mas = Math.Sqrt(num5);
			Correlation = num4 / Math.Sqrt(num3 * num5);
			if (Correlation > 0.99)
			{
				Correlation = 0.99;
			}
			else if (Correlation < -0.99)
			{
				Correlation = -0.99;
			}
			rmsStarRA_Total_mas = Utilities.QuadratureAddition(Utilities.QuadratureAddition(rmsRAstar_mas, rmsRAFrameRotate_mas), rmsRADoubleOffset_mas);
			rmsStarDec_Total_mas = Utilities.QuadratureAddition(Utilities.QuadratureAddition(rmsDecStar_mas, rmsDecFrameRotate_mas), rmsDecDoubleOffset_mas);
		}

		internal static string FindAsteroidOccultedStars(double RAstar, double DecStar, out bool AnotherEventWithStarFound, out string StarId_Of_Found)
		{
			return FindAsteroidOccultedStars(RAstar, DecStar, "", 0, 0, out AnotherEventWithStarFound, out StarId_Of_Found);
		}

		internal static string FindAsteroidOccultedStars(double RAstar, double DecStar, string StarCat, int ZoneOrNum, int Num, out bool AnotherEventWithStarFound, out string StarId_Of_Found)
		{
			StarId_Of_Found = "";
			string text = "Asteroidal occultations of the star at RA " + Utilities.DEGtoDMS(RAstar * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false) + ", Dec " + Utilities.DEGtoDMS(DecStar * (180.0 / Math.PI), 3, 0, MinutesOnly: false) + "\r\n   Asteroid               Date            Star\r\n";
			if (double.IsNaN(RAstar) | double.IsNaN(DecStar))
			{
				text += "Invalid star coordinates";
				AnotherEventWithStarFound = false;
				return text;
			}
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			double Distance = 0.0;
			double num4 = 0.0;
			string SourceCode = "";
			AnotherEventWithStarFound = false;
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count == 0)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			double PA_atOrigin;
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				Data_and_Plots.Historical_AllEvents.GetStarCoords(i, out var StarRA, out var StarDec, out SourceCode);
				Utilities.Distance(StarRA, StarDec, RAstar, DecStar, out Distance, out PA_atOrigin);
				if (Distance < 0.0001)
				{
					num++;
					AnotherEventWithStarFound = true;
					text += Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: true, Kepler2: false);
					text = text + "Observers :\r\n  " + Data_and_Plots.Historical_AllEvents.GetObserverNames(i) + "\r\n";
					StarId_Of_Found = Data_and_Plots.Historical_AllEvents.GetStarID(i);
				}
			}
			if (num == 0)
			{
				text += "No asteroidal events were found for this star\r\n";
			}
			else
			{
				switch (StarCat)
				{
				case "Hip":
					num3 = LightData.HipCount(ZoneOrNum);
					break;
				case "Tycho2":
					num3 = LightData.Tyc2Count(ZoneOrNum, Num);
					break;
				case "UCAC4":
					num3 = LightData.U4Count(ZoneOrNum, Num);
					break;
				}
				text = text + "\r\n*** " + num3 + " asteroidal occultation light curves were found for this star ***\r\n";
			}
			text += "\r\nLunar occultations\r\n";
			if (HistoryFiles.Count < 1)
			{
				string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
				foreach (string path in files)
				{
					HistoryFiles.Add(Path.GetFileName(path));
				}
			}
			if (LunarOccultations.XZIndex[2] == 0)
			{
				LunarOccultations.InitialiseStarCatIndexArray(AppPath + "\\Resource Files\\XZ80.dat");
			}
			int num5 = LunarOccultations.XZIndex[(int)Math.Floor(RAstar * (180.0 / Math.PI))];
			int num6 = LunarOccultations.XZIndex[(int)Math.Ceiling(RAstar * (180.0 / Math.PI))];
			string text2 = "";
			int num7 = 0;
			num3 = 0;
			bool flag = false;
			using FileStream fileStream = new FileStream(Utilities.AppPath + "/Resource Files/XZ80.dat", FileMode.Open, FileAccess.Read);
			using BinaryReader readXZ = new BinaryReader(fileStream);
			int num8 = num5;
			do
			{
				XZ80Q.ReadStarEntry(fileStream, readXZ, num8);
				double rA_rad = XZ80Q.RA_rad;
				num4 = XZ80Q.Dec_rad;
				Utilities.Distance(rA_rad, num4, RAstar, DecStar, out Distance, out PA_atOrigin);
				if (Distance < 0.0001)
				{
					num2++;
					num7 += LightData.XZ80Count(XZ80Q.XZ);
					int num9 = 0;
					int result = 0;
					flag = false;
					string text3 = "Star is XZ " + XZ80Q.XZ;
					num9 = XZ80Q.XZ;
					text2 = "X";
					if (XZ80Q.SAO > 0)
					{
						text2 = "S";
						text3 = text3 + " = SAO " + XZ80Q.SAO;
						num9 = XZ80Q.SAO;
					}
					if (XZ80Q.ZC > 0)
					{
						text2 = "R";
						text3 = text3 + " = R " + XZ80Q.ZC;
						num9 = XZ80Q.ZC;
					}
					num7 = 0;
					for (int k = 0; k < HistoryFiles.Count; k++)
					{
						using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[k]!.ToString());
						do
						{
							string text4 = streamReader.ReadLine();
							if (int.TryParse(text4.Substring(19, 6), out result) && text4.Substring(18, 1) == text2 && result == num9)
							{
								flag = true;
								num7++;
							}
						}
						while (!streamReader.EndOfStream);
					}
					if (flag)
					{
						num3 = LightData.XZ80Count(XZ80Q.XZ);
					}
					text3 = ((num7 <= 0) ? (text3 + " :  No lunar occultations found\r\n") : (text3 + " :  " + num7 + " lunar occults,  " + num3 + " light curves"));
					text += text3;
				}
				num8++;
			}
			while (num8 < num6);
			if (num2 == 0)
			{
				return text + "No equivalent XZ star found\r\n";
			}
			return text;
		}

		private static int GetLightCurveCount(string LightCurveFile, int Find_StarNum_or_Zone, int Find_StarNumInZone)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			string text = "";
			int num4 = 0;
			int num5 = 0;
			bool flag = LightCurveFile.Contains("T2Index.txt") | LightCurveFile.Contains("U4Index.txt");
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + LightCurveFile);
			do
			{
				text = streamReader.ReadLine();
				num4 = text.IndexOf("-");
				if (flag)
				{
					if (num4 > 0)
					{
						num2 = int.Parse(text.Substring(0, num4));
					}
					if (num2 == Find_StarNum_or_Zone)
					{
						num5 = text.IndexOf("-", num4 + 1);
						if (num5 > 0)
						{
							num3 = int.Parse(text.Substring(num4 + 1, num5 - num4 - 1));
						}
						if (num3 == Find_StarNumInZone)
						{
							num++;
						}
					}
				}
				else
				{
					if (num4 > 0)
					{
						num2 = int.Parse(text.Substring(0, num4));
					}
					if (num2 == Find_StarNum_or_Zone)
					{
						num++;
					}
				}
			}
			while (!streamReader.EndOfStream);
			return num;
		}

		public static void ShowFindOccultedStars()
		{
			try
			{
				((Control)FindOccultedStars).Show();
			}
			catch
			{
				FindOccultedStars = new Statistics();
				((Control)FindOccultedStars).Show();
			}
		}

		public static string FindDuplicatedStars_plus_MagStats(bool SortByStarNumber)
		{
			int[] array = new int[20];
			int num = 0;
			int result = 0;
			int num2 = 17;
			string SourceCode = "";
			string[] array2 = new string[10] { "Hip2", "Gaia DR1", "Gaia DR2", "Gaia EDR3", "Gaia DR3", "", "", "", "", "UBSC" };
			List<Asteroid_Observations_StarMatch> list = new List<Asteroid_Observations_StarMatch>();
			List<Asteroid_Observations_StarMatch> list2 = new List<Asteroid_Observations_StarMatch>();
			List<Asteroid_Observations_StarMatch> list3 = new List<Asteroid_Observations_StarMatch>();
			List<Asteroid_Observations_StarMatch> list4 = new List<Asteroid_Observations_StarMatch>();
			List<Asteroid_Observations_StarMatch> list5 = new List<Asteroid_Observations_StarMatch>();
			StarList.Clear();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				Asteroid_Observations_StarMatch asteroid_Observations_StarMatch = new Asteroid_Observations_StarMatch();
				Data_and_Plots.Historical_AllEvents.GetStarCoords(i, out var StarRA, out var StarDec, out SourceCode, out var StarMag, out var StarCat, out var CatNumber, out var _);
				asteroid_Observations_StarMatch.RA = StarRA;
				asteroid_Observations_StarMatch.Dec = StarDec;
				asteroid_Observations_StarMatch.Mag = StarMag;
				asteroid_Observations_StarMatch.Date = Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(i);
				asteroid_Observations_StarMatch.Description = Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: true, Kepler2: false);
				asteroid_Observations_StarMatch.ID += Utilities.StarIdentifier_ToMag6(StarRA * (180.0 / Math.PI), StarDec * (180.0 / Math.PI), StarMag, WithConstellation: true);
				string text = "";
				if (StarCat.ToUpper() == "HIP")
				{
					text = Utilities.StarNameFromHip(int.Parse(CatNumber), StarMag);
				}
				if (text.Length > 0)
				{
					asteroid_Observations_StarMatch.ID = asteroid_Observations_StarMatch.ID + ",  " + text;
				}
				int.TryParse(SourceCode, out result);
				asteroid_Observations_StarMatch.Source = array2[result];
				StarList.Add(asteroid_Observations_StarMatch);
				if (StarMag < 2.0)
				{
					list.Add(asteroid_Observations_StarMatch);
				}
				else if (StarMag < 3.0)
				{
					list2.Add(asteroid_Observations_StarMatch);
				}
				else if (StarMag < 4.0)
				{
					list3.Add(asteroid_Observations_StarMatch);
				}
				else if (StarMag < 5.0)
				{
					list4.Add(asteroid_Observations_StarMatch);
				}
				else if (StarMag < 6.0)
				{
					list5.Add(asteroid_Observations_StarMatch);
				}
				num = (int)StarMag;
				if ((double)num < 1.99)
				{
					num = 1;
				}
				if (num > num2)
				{
					num = num2;
				}
				array[num]++;
			}
			Asteroid_Observations_StarMatch.SortByStarNumber = false;
			StarList.Sort();
			if (SortByStarNumber)
			{
				Asteroid_Observations_StarMatch.SortByStarNumber = SortByStarNumber;
				list.Sort();
				list2.Sort();
				list3.Sort();
				list4.Sort();
				list5.Sort();
			}
			string text2 = "Asteroidal occultations\r\nList of stars occulted more than once\r\n";
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			int num5;
			for (num5 = 0; num5 < StarList.Count; num5++)
			{
				double rA = StarList[num5].RA;
				num3 = 0;
				num4 = 0;
				flag = false;
				do
				{
					num3++;
					if (num5 + num3 >= StarList.Count || StarList[num5 + num3].RA - rA > 0.0001)
					{
						break;
					}
					if (Math.Abs(StarList[num5 + num3].Dec - StarList[num5].Dec) < 0.0001 && StarList[num5 + num3].Date != StarList[num5].Date)
					{
						if (!flag)
						{
							flag = true;
							text2 = text2 + "\r\n" + StarList[num5].Description;
						}
						text2 += StarList[num5 + num3].Description;
						if (num3 - num4 == 1)
						{
							num4++;
						}
					}
				}
				while (num5 + num3 < StarList.Count);
				num5 += num4;
			}
			text2 += "\r\n\r\nCount of occulted stars by Magnitude\r\nMagnitude range   Num.\r\n";
			text2 += string.Format("          <2.0   {0,4}\r\n", array[1]);
			for (int j = 2; j < num2; j++)
			{
				text2 += string.Format("  {0,4:f1} to {1,4:f1}   {2,4}\r\n", j, (double)j + 0.9, array[j]);
			}
			text2 += string.Format("         >" + string.Format("{0,1:f1}", num2) + "   {0,4}\r\n", array[num2]);
			text2 += "\r\n\r\nList of bright star events\r\n";
			text2 += "\r\nStars brighter than mag. 2.0\r\n";
			for (int k = 0; k < list.Count; k++)
			{
				text2 += list[k].DescriptionWithSource;
			}
			text2 += "\r\nStars between mag. 2.0 and 3.0\r\n";
			for (int l = 0; l < list2.Count; l++)
			{
				text2 += list2[l].DescriptionWithSource;
			}
			text2 += "\r\nStars between mag. 3.0 and 4.0\r\n";
			for (int m = 0; m < list3.Count; m++)
			{
				text2 += list3[m].DescriptionWithSource;
			}
			text2 += "\r\nStars between mag. 4.0 and 5.0\r\n";
			for (int n = 0; n < list4.Count; n++)
			{
				text2 += list4[n].DescriptionWithSource;
			}
			text2 += "\r\nStars between mag. 5.0 and 6.0\r\n";
			for (int num6 = 0; num6 < list5.Count; num6++)
			{
				text2 += list5[num6].DescriptionWithSource;
			}
			return text2;
		}

		public static string FindAsterodsInOccultations()
		{
			AsteroidsObserved.Clear();
			List<string> list = new List<string>();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				AsteroidNumbers asteroidNumbers = new AsteroidNumbers();
				Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
				int.TryParse(AsteroidNumber, out var result);
				if (result >= 1)
				{
					asteroidNumbers.AsteroidNumber = result;
					asteroidNumbers.AsteroidName = AsteroidName;
					asteroidNumbers.EventCount = 1;
					AsteroidsObserved.Add(asteroidNumbers);
				}
			}
			AsteroidNumbers.Sort = 0;
			AsteroidsObserved.Sort();
			int num = AsteroidsObserved.Count - 2;
			do
			{
				if (AsteroidsObserved[num].AsteroidNumber == AsteroidsObserved[num + 1].AsteroidNumber)
				{
					AsteroidsObserved[num].EventCount += AsteroidsObserved[num + 1].EventCount;
					AsteroidsObserved.RemoveAt(num + 1);
				}
				else
				{
					num--;
				}
			}
			while (num >= 0);
			string text = string.Format("List of the {0,1} asteroids that have been observed in 1 or more occultations\r\n\r\n", AsteroidsObserved.Count);
			int num2 = 0;
			bool flag = false;
			for (int j = 0; j < AsteroidsObserved.Count; j++)
			{
				if (!flag)
				{
					if (AsteroidsObserved[j].AsteroidNumber - num2 > 1)
					{
						flag = true;
						text += string.Format("Every asteroid in the range 1 to {0,1} has been observed in at least 1 occultation event\r\n\r\n", num2);
					}
					else
					{
						num2 = AsteroidsObserved[j].AsteroidNumber;
					}
				}
				list.Add(AsteroidsObserved[j].AsteroidNumber.ToString().PadLeft(6) + " " + AsteroidsObserved[j].AsteroidName.PadRight(16) + AsteroidsObserved[j].EventCount.ToString().PadLeft(3));
			}
			AsteroidNumbers.Sort = 1;
			AsteroidsObserved.Sort();
			for (int k = 0; k < AsteroidsObserved.Count; k++)
			{
				list[k] = list[k] + "    " + AsteroidsObserved[k].AsteroidNumber.ToString().PadLeft(6) + " " + AsteroidsObserved[k].AsteroidName.PadRight(16) + AsteroidsObserved[k].EventCount.ToString().PadLeft(3);
			}
			text += "Count      by Asteroid Number   #        by Number of events  #\r\n";
			for (int l = 0; l < list.Count; l++)
			{
				if (l % 5 == 0)
				{
					text += "\r\n";
				}
				text = text + string.Format("{0,5}  ", l + 1) + list[l] + "\r\n";
			}
			return text;
		}

		public static string SmallMagDropEvents(double MagDrop)
		{
			List<SmallMags> list = new List<SmallMags>();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				double starMag = Data_and_Plots.Historical_AllEvents.GetStarMag(i);
				double asteroidMag = Data_and_Plots.Historical_AllEvents.GetAsteroidMag(i);
				double num = -2.5 * Math.Log10(Math.Pow(10.0, starMag / -2.5) + Math.Pow(10.0, asteroidMag / -2.5));
				double num2 = asteroidMag - num;
				if (num2 < MagDrop)
				{
					Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
					if (!((AsteroidNumber.Substring(1, 1) == "P") & (AsteroidNumber.PadRight(6).Substring(3, 3) == "M00")) && !((AsteroidNumber == " P6M06") | (AsteroidNumber == " P5M01") | (AsteroidNumber == " P5M02") | (AsteroidNumber == " P5M03") | (AsteroidNumber == " P5M04")))
					{
						SmallMags smallMags = new SmallMags();
						smallMags.Drop = num2;
						smallMags.Label = string.Format("{0,1:f2}   ", num2) + Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(i) + " " + AsteroidNumber + " " + AsteroidName;
						list.Add(smallMags);
					}
				}
			}
			list.Sort();
			string text = string.Format("{0,1} asteroid events with Mag drop less than {1,1:f2}\r\n\r\n", list.Count, MagDrop) + "Mdrop      Date        Object\r\n";
			for (int j = 0; j < list.Count; j++)
			{
				text = text + list[j].Label + "\r\n";
				if ((j + 1) % 5 == 0)
				{
					text += "\r\n";
				}
			}
			return text;
		}

		public static string SmallDiameterEvents(double Diameter)
		{
			List<SmallDias> list = new List<SmallDias>();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				double asteroidNominalDiameter = Data_and_Plots.Historical_AllEvents.GetAsteroidNominalDiameter(i);
				if (Diameter >= asteroidNominalDiameter)
				{
					Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
					SmallDias smallDias = new SmallDias();
					smallDias.Dia = asteroidNominalDiameter;
					smallDias.Label = string.Format("{0,1:f1}   ", asteroidNominalDiameter) + Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(i) + " " + AsteroidNumber + " " + AsteroidName;
					list.Add(smallDias);
				}
			}
			list.Sort();
			string text = string.Format("{0,1} asteroid events with assumed diameter less than {1,1:f1} km\r\n\r\n", list.Count, Diameter) + "Dia km      Date        Object\r\n";
			for (int j = 0; j < list.Count; j++)
			{
				text = text + list[j].Label + "\r\n";
				if ((j + 1) % 5 == 0)
				{
					text += "\r\n";
				}
			}
			return text;
		}

		public static string GetSlowEvents()
		{
			string text = "Slow Events. Across-Earth Duration > 90 mins\r\n\r\nDuration   Diameter        Date        Object\r\n";
			List<SlowEvents> list = new List<SlowEvents>();
			double num = 0.0;
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				num = 2.0 / Data_and_Plots.Historical_AllEvents.GetMotion_n(i);
				if (num > 1.5 || num < 0.1)
				{
					Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
					double asteroidNominalDiameter = Data_and_Plots.Historical_AllEvents.GetAsteroidNominalDiameter(i);
					SlowEvents slowEvents = new SlowEvents();
					slowEvents.Duration = num;
					if (num > 1.0)
					{
						slowEvents.Label = string.Format("{0,4:f1} hrs {1,7:f1} km    ", num, asteroidNominalDiameter) + Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(i) + " " + AsteroidNumber + " " + AsteroidName;
					}
					else
					{
						slowEvents.Label = string.Format("{0,4:f1} min {1,7:f1} km    ", num * 60.0, asteroidNominalDiameter) + Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(i) + " " + AsteroidNumber + " " + AsteroidName;
					}
					list.Add(slowEvents);
				}
			}
			list.Sort();
			int num2 = -1;
			for (int j = 0; j < list.Count; j++)
			{
				num2++;
				if (j > 1 && ((list[j - 1].Duration > 1.0) & (list[j].Duration < 1.0)))
				{
					text += "\r\n\r\nFast Events. Across-Earth Duration < 360 seconds\r\n\r\nDuration   Diameter        Date        Object\r\n";
					num2 = 0;
				}
				text = text + list[j].Label + "\r\n";
				if ((num2 + 1) % 5 == 0)
				{
					text += "\r\n";
				}
			}
			return text;
		}

		public static void ListEventsWithNoMainBody()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			string[] array = Data_and_Plots.Historical_AllEvents.GetAsteroidEvents_NoMainBody().Replace("\r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_NoMainBody).Show();
			}
			catch
			{
				Display_NoMainBody = new DisplayNoMainBody();
				((Control)Display_NoMainBody).Show();
			}
			Display_NoMainBody.lstNoMainBody.get_Items().Clear();
			Display_NoMainBody.lstNoMainBody.get_Items().Add((object)"Events where the primary body was not observed");
			Display_NoMainBody.lstNoMainBody.get_Items().Add((object)"");
			for (int i = 0; i < array.Length; i++)
			{
				Display_NoMainBody.lstNoMainBody.get_Items().Add((object)array[i]);
			}
		}

		public static string GetRegionalStats(int StartYear, int EndYear, bool SaveEventIdentifiers)
		{
			string text = StartYear + " to " + EndYear;
			string[] array = new string[6] { "Australasia ", "East Asia   ", "Europe      ", "Nth America ", "Sth America ", "World       " };
			string text2 = "".PadRight(20) + "Asteroidal occultations statistics for the period " + text + "\r\n";
			stats = new List<RegionalStats>();
			for (int i = 0; i < 6; i++)
			{
				stats.Add(new RegionalStats());
			}
			Stat[0].LongWest = 90;
			Stat[0].LongEast = 180;
			Stat[0].LatNorth = 0;
			Stat[0].LatSouth = -60;
			Stat[1].LongWest = 90;
			Stat[1].LongEast = 170;
			Stat[1].LatNorth = 80;
			Stat[1].LatSouth = 0;
			Stat[2].LongWest = -20;
			Stat[2].LongEast = 90;
			Stat[2].LatNorth = 80;
			Stat[2].LatSouth = 0;
			Stat[3].LongWest = -180;
			Stat[3].LongEast = -50;
			Stat[3].LatNorth = 80;
			Stat[3].LatSouth = 0;
			Stat[4].LongWest = -83;
			Stat[4].LongEast = -34;
			Stat[4].LatNorth = -10;
			Stat[4].LatSouth = -55;
			Stat[5].LongWest = -180;
			Stat[5].LongEast = 360;
			Stat[5].LatNorth = 90;
			Stat[5].LatSouth = -90;
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int Year = 0;
			bool flag = false;
			string EventID = "";
			IDforRegions = new List<string>[6];
			for (int j = 0; j < 6; j++)
			{
				IDforRegions[j] = new List<string>();
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count == 0)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int k = 0; k < Data_and_Plots.Historical_AllEvents.OccEvents.Count; k++)
			{
				Data_and_Plots.Historical_AllEvents.GetEventDate(k, out Year, out var Month, out var _);
				if (!(Year >= StartYear && Year <= EndYear))
				{
					continue;
				}
				num++;
				flag = false;
				Data_and_Plots.Historical_AllEvents.GetMainObserverLocation(k, out var Longitude, out var Latitude, out var StarIsDouble, out var AsteroidIsDouble, out EventID, out var AsteroidDiameter, out var MagDrop, out var NumChords);
				for (int l = 0; l < 6; l++)
				{
					_ = 5;
					if ((Longitude > (double)Stat[l].LongWest) & (Longitude < (double)Stat[l].LongEast) & (Latitude < (double)Stat[l].LatNorth) & (Latitude > (double)Stat[l].LatSouth))
					{
						flag = true;
						Stat[l].DiaAll++;
						if (AsteroidDiameter < 3.0)
						{
							Stat[l].Dia3++;
						}
						else if (AsteroidDiameter < 10.0)
						{
							Stat[l].Dia3_10++;
						}
						else if (AsteroidDiameter < 20.0)
						{
							Stat[l].Dia10_20++;
						}
						else if (AsteroidDiameter < 50.0)
						{
							Stat[l].Dia20_50++;
						}
						else if (AsteroidDiameter < 100.0)
						{
							Stat[l].Dia50_100++;
						}
						else
						{
							Stat[l].Dia100_++;
						}
						if (MagDrop < 0.2)
						{
							Stat[l].MagDrop_02++;
						}
						else if (MagDrop < 0.5)
						{
							Stat[l].MagDrop02_05++;
						}
						else if (MagDrop < 1.0)
						{
							Stat[l].MagDrop05_10++;
						}
						else
						{
							Stat[l].MagDrop10_++;
						}
						if (NumChords >= 10)
						{
							Stat[l].Chords10_++;
						}
						else if (NumChords >= 4)
						{
							Stat[l].Chords4_9++;
						}
						else if (NumChords > 1)
						{
							Stat[l].Chords2_3++;
						}
						else if (NumChords == 1)
						{
							Stat[l].Chords1_++;
						}
						Stat[l].Months[Month]++;
						if (l < 5)
						{
							IDforRegions[l].Add(EventID);
						}
						if (l < 4)
						{
							l = 4;
						}
					}
				}
				if (!flag)
				{
					IDforRegions[4].Add(EventID);
				}
				if (StarIsDouble)
				{
					num2++;
				}
				if (AsteroidIsDouble)
				{
					num3++;
				}
			}
			List<string> list = new List<string>();
			for (int m = 0; m < 6; m++)
			{
				list.Add(string.Format(":{0,6} |  {1,4}     {2,4}     {3,4}     {4,4}     {5,4}    {6,4}  | {7,4}    {8,4}     {9,4}    {10,4}  | {11,4}  {12,4}  {13,3} {14,3}\r\n", Stat[m].DiaAll, Stat[m].Dia3, Stat[m].Dia3_10, Stat[m].Dia10_20, Stat[m].Dia20_50, Stat[m].Dia50_100, Stat[m].Dia100_, Stat[m].MagDrop_02, Stat[m].MagDrop02_05, Stat[m].MagDrop05_10, Stat[m].MagDrop10_, Stat[m].Chords1_, Stat[m].Chords2_3, Stat[m].Chords4_9, Stat[m].Chords10_));
			}
			text2 += "\r\n\r\nNumbers for diameter range, magnitude drop, and number of chords";
			text2 += $"\r\n\r\n                    |              By diameter range (km)                |        by magnitude drop       |     Number of chords";
			text2 += $"\r\nRegion        Total |    <3km   3-10km  10-20km  20-50km 50-100km  >100km|  <0.2  0.2-0.5  0.5-1.0  >1.0  |    1   2-3  4-9  10+\r\n";
			text2 = text2 + "".PadRight(128, '-') + "\r\n";
			text2 = text2 + array[5] + list[5] + "\r\n";
			for (int n = 0; n < 5; n++)
			{
				text2 = text2 + array[n] + list[n];
			}
			text2 += string.Format("Other       :{0,6}\r\n", num - Stat[0].DiaAll - Stat[1].DiaAll - Stat[2].DiaAll - Stat[3].DiaAll - Stat[4].DiaAll);
			bool flag2 = false;
			if ((StartYear == DateTime.Now.Year) | ((StartYear == DateTime.Now.Year - 1) & (DateTime.Now.Month < 4)))
			{
				flag2 = true;
			}
			text2 = ((!flag2) ? (text2 + "\r\n\r\nNumber of observations by month, over the period " + text + "\r\n") : (text2 + "\r\n\r\nNumber of observations by month in " + StartYear + ", that have been finalized and entered into Occult - as at " + DateTime.Now.Year + " " + Utilities.ShortMonths[DateTime.Now.Month] + " " + DateTime.Now.Day.ToString().PadLeft(2, '0') + "\r\n"));
			text2 += $"\r\nRegion        Total   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec\r\n";
			text2 += "".PadRight(91, '-');
			for (int num4 = 0; num4 < 6; num4++)
			{
				int num5 = ((num4 != 0) ? (num4 - 1) : 5);
				string text3 = array[num5] + ":" + string.Format("{0, 6}", Stat[num5].DiaAll);
				for (int num6 = 1; num6 < 13; num6++)
				{
					text3 += string.Format("{0, 6}", Stat[num5].Months[num6]);
				}
				text2 = text2 + "\r\n" + text3;
				if (num4 == 0)
				{
					text2 += "\r\n";
				}
			}
			text2 += "\r\n\r\n".PadRight(27, '-');
			text2 += string.Format("\r\nDouble stars      : {0,3}", num2);
			text2 += string.Format("\r\nBinary asteroids  : {0,3}", num3);
			int observationsCount = GetObservationsCount(StartYear, EndYear);
			text2 += "\r\n\r\n".PadRight(37, '-');
			text2 += string.Format("\r\nIndividual observations  : {0,6}\r\n", observationsCount);
			double num7 = Utilities.JD_from_Date(StartYear, 1, 0.0);
			double num8 = Utilities.JD_from_Date(EndYear, 12, 32.0);
			List<int> list2 = new List<int>();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int num9 = 0; num9 < Data_and_Plots.Historical_AllEvents.OccEvents.Count; num9++)
			{
				if (((Data_and_Plots.Historical_AllEvents.OccEvents[num9].EventJD > num7) & (Data_and_Plots.Historical_AllEvents.OccEvents[num9].EventJD < num8)) && Data_and_Plots.Historical_AllEvents.OccEvents[num9].AstNum > 0)
				{
					list2.Add(Data_and_Plots.Historical_AllEvents.OccEvents[num9].AstNum);
				}
			}
			list2.Sort();
			for (int num10 = list2.Count - 1; num10 > 0; num10--)
			{
				if (list2[num10] == list2[num10 - 1])
				{
					list2.RemoveAt(num10);
				}
			}
			text2 += "\r\n".PadRight(81, '-');
			text2 += string.Format("\r\nNumbered asteroids observed  :  {0,1:f0}\r\n\r\n", list2.Count);
			for (int num11 = 0; num11 < list2.Count; num11++)
			{
				text2 = text2 + list2[num11].ToString().PadLeft(7) + " ";
				if ((num11 + 1) % 10 == 0)
				{
					text2 += "\r\n";
				}
			}
			if (SaveEventIdentifiers)
			{
				string text4 = "_Increasing.txt";
				for (int num12 = 0; num12 <= 1; num12++)
				{
					if (num12 > 0)
					{
						text4 = "_Decreasing.txt";
					}
					for (int num13 = 0; num13 < 5; num13++)
					{
						using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated Files\\" + array[num13].Trim() + " Events " + text + text4);
						streamWriter.WriteLine(IDforRegions[num13].Count + " events for " + array[num13].Trim() + ", " + text);
						if (num12 == 0)
						{
							for (int num14 = 0; num14 < IDforRegions[num13].Count; num14++)
							{
								if (num14 % 5 == 0)
								{
									streamWriter.WriteLine();
								}
								streamWriter.WriteLine(string.Format("{0,4} : ", num14 + 1) + IDforRegions[num13][num14]);
							}
							continue;
						}
						for (int num15 = IDforRegions[num13].Count - 1; num15 >= 0; num15--)
						{
							if ((IDforRegions[num13].Count - num15 - 1) % 5 == 0)
							{
								streamWriter.WriteLine();
							}
							streamWriter.WriteLine(string.Format("{0,4}  {1,4} : ", num15 + 1, IDforRegions[num13].Count - num15) + IDforRegions[num13][num15]);
						}
					}
				}
			}
			return text2;
		}

		public static int GetObservationsCount(int StartYear, int EndYear)
		{
			int num = 0;
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count == 0)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				Data_and_Plots.Historical_AllEvents.GetEventDate(i, out var Year, out var _, out var _);
				if (!(Year >= StartYear && Year <= EndYear))
				{
					continue;
				}
				for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains(Tags.TagEnd[20]))
					{
						num++;
					}
				}
			}
			return num;
		}

		public static void ListDuplicates()
		{
			string text = "";
			string text2 = "";
			string text3 = "  ";
			ShowErrors();
			((Control)Display_Errors).set_Width(500);
			((Control)Display_Errors).set_Text("Display possible errors in Observations file");
			Display_Errors.lstErrors.get_Items().Clear();
			Display_Errors.lstErrors.get_Items().Add((object)"List of possible duplicate entries");
			Display_Errors.lstErrors.get_Items().Add((object)"");
			Display_Errors.lstErrors.get_Items().Add((object)"      Asteroid           Year Mth Dy        Star");
			Display_Errors.lstErrors.get_Items().Add((object)"");
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			text2 = Data_and_Plots.Historical_AllEvents.GetEventID(0, IncludeMagnitude: false, Kepler2: false);
			for (int i = 1; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				text = Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: false, Kepler2: false);
				if (text.Substring(0, 34) == text2.Substring(0, 34))
				{
					text3 = ((!(text == text2)) ? "  " : "* ");
					Display_Errors.lstErrors.get_Items().Add((object)(text3 + text2));
					Display_Errors.lstErrors.get_Items().Add((object)(text3 + text));
					Display_Errors.lstErrors.get_Items().Add((object)"");
				}
				text2 = text;
			}
		}

		private static void ShowErrors()
		{
			try
			{
				((Control)Display_Errors).Show();
			}
			catch
			{
				Display_Errors = new DisplayErrors();
				((Control)Display_Errors).Show();
			}
			((Control)Display_Errors).Focus();
		}

		public static void ListUnfittedEvents()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			int Count;
			string[] array = Data_and_Plots.Historical_AllEvents.GetUnfittedEvents(out Count).Replace("\r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_Unfitted).Show();
			}
			catch
			{
				Display_Unfitted = new DisplayUnfitted();
				((Control)Display_Unfitted).Show();
			}
			((Control)Display_Unfitted).Focus();
			Display_Unfitted.lstUnfitted.get_Items().Clear();
			Display_Unfitted.lstUnfitted.get_Items().Add((object)string.Format("List of {0,1} events that have not been fitted", Count));
			Display_Unfitted.lstUnfitted.get_Items().Add((object)"");
			for (int i = 0; i < array.Length; i++)
			{
				Display_Unfitted.lstUnfitted.get_Items().Add((object)array[i]);
			}
		}

		public static void ListEventsForReview()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			int Count;
			string[] array = Data_and_Plots.Historical_AllEvents.GetEventsForReview(out Count).Replace("\r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_ForReview).Show();
			}
			catch
			{
				Display_ForReview = new DisplayForReview();
				((Control)Display_ForReview).Show();
			}
			Display_ForReview.lstForReview.get_Items().Clear();
			Display_ForReview.lstForReview.get_Items().Add((object)string.Format("           {0,1} events for review", Count));
			Display_ForReview.lstForReview.get_Items().Add((object)"");
			for (int i = 0; i < array.Length; i++)
			{
				Display_ForReview.lstForReview.get_Items().Add((object)array[i]);
			}
		}

		public static void ListWrongUncertainties()
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			double UncertaintyRatio;
			string[] array = Data_and_Plots.Historical_AllEvents.GetBadUncertainties(out UncertaintyRatio).Replace("/r", "").Split(new char[1] { '\n' });
			try
			{
				((Control)Display_LargeUncerts).Show();
			}
			catch
			{
				Display_LargeUncerts = new DisplayLargeUncert();
				((Control)Display_LargeUncerts).Show();
			}
			Display_LargeUncerts.lstLargeUncert.get_Items().Clear();
			Display_LargeUncerts.lstLargeUncert.get_Items().Add((object)("Events with uncertainty > " + string.Format("{0,1:f2}*diameter", UncertaintyRatio)));
			Display_LargeUncerts.lstLargeUncert.get_Items().Add((object)"");
			Display_LargeUncerts.lstLargeUncert.get_Items().Add((object)"Year  m  d Asteroid   Dia    e_RA    e_Dec");
			Display_LargeUncerts.lstLargeUncert.get_Items().Add((object)"                      \"       \"       \"");
			for (int i = 0; i < array.Length; i++)
			{
				Display_LargeUncerts.lstLargeUncert.get_Items().Add((object)array[i].ToString());
			}
		}

		public static void Show_MPCandPDS()
		{
			try
			{
				((Control)MPC_PDS).Show();
			}
			catch
			{
				MPC_PDS = new MPCandPDS();
				((Control)MPC_PDS).Show();
			}
		}

		public static void Show_EditPDS_Headers()
		{
			try
			{
				((Control)EditPDS_Headers).Show();
			}
			catch
			{
				EditPDS_Headers = new PDS_Headers();
				((Control)EditPDS_Headers).Show();
			}
		}
	}
}
