using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Shapes;

namespace Occult.Asteroid_Observations
{
	internal class AllEvents
	{
		public List<LinesForAnEvent> OccEvents = new List<LinesForAnEvent>();

		public LinesForAnEvent LinesOfAnEvent = new LinesForAnEvent();

		public LinesForAnEvent NewLines;

		internal static List<HistoricalIndexData> HistoricalIndex = new List<HistoricalIndexData>();

		private double VersionOfReadFile = 1.0;

		internal const double XYAB_1_DecimalPlace_Test_km = 5.0;

		internal const double XYAB_2_DecimalPlace_Test_km = 0.5;

		private static string DecimalPlacesInSolution_Output = "1";

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

		internal static int DecimalPlaces_Int_InSolution_Output(double MinorAxis)
		{
			if (MinorAxis > 5.0)
			{
				return 1;
			}
			if (MinorAxis > 0.5)
			{
				return 2;
			}
			return 3;
		}

		internal static string DecimalPlaces_String_InSolution_Output(double MinorAxis)
		{
			return DecimalPlaces_Int_InSolution_Output(MinorAxis).ToString();
		}

		internal void ReadObservationsFile(bool HistoricalFile, string SingleFileName)
		{
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c2: Invalid comparison between Unknown and I4
			//IL_0208: Unknown result type (might be due to invalid IL or missing references)
			OccEvents.Clear();
			Data_and_Plots.CreatePlotForm();
			int result = 0;
			int result2 = 0;
			double result3 = 0.0;
			double Hour = 0.0;
			string text = "";
			string text2 = "";
			string path = "";
			string text3 = "";
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			bool flag2 = true;
			bool flag3 = true;
			bool obsFileisCurrentVersion = true;
			DisplayMPOccultations.LoadISAM_and_DAMIT_ids(UpdateISAM: true);
			string text4 = Utilities.AppPath + "\\Resource Files\\Asteroid_Observations.dat";
			string text5 = Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile;
			if (HistoricalFile)
			{
				bool flag4 = File.Exists(text4);
				bool flag5 = File.Exists(text5);
				path = ((flag4 && flag5) ? (((int)MessageBox.Show("Both TEXT and XML versions of the Historical file of observations are present on your computer\r\n\r\nDo you want to load the XML version in preference to the text version?", "Select historical file version", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 6) ? text4 : text5) : ((!flag4) ? text5 : text4));
			}
			if (!HistoricalFile)
			{
				path = SingleFileName;
			}
			using (StreamReader streamReader = new StreamReader(path, Encoding.ASCII))
			{
				flag3 = true;
				num4 = 0;
				do
				{
					text = streamReader.ReadLine();
					if (text.Trim().Length == 0)
					{
						continue;
					}
					if (text.Contains("<FileVersion>"))
					{
						text3 = text.Replace("<FileVersion>", "").Replace("</FileVersion>", "").Trim();
						double.TryParse(text3, out VersionOfReadFile);
						if (text3 != Tags.ObservationsFileVersion)
						{
							obsFileisCurrentVersion = false;
							double.TryParse(Tags.ObservationsFileVersion, out var result4);
							string text6 = "";
							string text7 = "";
							if (HistoricalFile)
							{
								text6 = "The version of the Asteroid Observations file on\r\nyour computer is inconsistent with your version of Occult";
								if (VersionOfReadFile > result4)
								{
									text7 = "You should update your version of Occult so that\r\nit is consistent with the asteroid Observations File";
								}
								if (VersionOfReadFile < result4)
								{
									text7 = "You should download the latest version of asteroid observations";
								}
								text7 += "\r\n\r\nYou will be able to continue, but you risk losing or currupting data if you try updating any observations files.";
							}
							else
							{
								text6 = "The version of the Asteroid Observations file you\r\nare opening is inconsistent with your version of Occult";
								if (VersionOfReadFile > result4)
								{
									text7 = "You should update your version of Occult so that\r\nit is consistent with the asteroid Observations File";
								}
								if (VersionOfReadFile < result4)
								{
									text7 = "While problems are unlikely, you need to carefully check the data read from the file";
								}
							}
							MessageBox.Show(text6 + "\r\n\r\n" + text7, "Inconsistent file version", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
						}
						else
						{
							obsFileisCurrentVersion = true;
						}
					}
					else
					{
						if ((text.Contains(Tags.TagFileStart) | text.Contains(Tags.TagFileEnd)) || ((text == "<Observations>") | (text == "</Observations>") | (text == "<Observations></Observations>")))
						{
							continue;
						}
						if ((text.Trim() == "") | text.Contains(Tags.TagEnd[0]))
						{
							if (text.Contains(Tags.TagEnd[0]))
							{
								LinesOfAnEvent.Lines.Add(text);
							}
							if (num3 > 0 && num4 < num3)
							{
								text2 = text2.Replace("#", "†");
							}
							LinesOfAnEvent.IndexLine = text2 + num.ToString().PadLeft(4) + " " + num2.ToString().PadLeft(3);
							LinesOfAnEvent.ChordCount = num2;
							LinesOfAnEvent.ObsFileisCurrentVersion = obsFileisCurrentVersion;
							LinesOfAnEvent.CurrentFileVersion = text3;
							OccEvents.Add(LinesOfAnEvent);
							flag = false;
							num = 0;
							num2 = 0;
							flag3 = true;
							num4 = 0;
							continue;
						}
						if (flag3)
						{
							LinesOfAnEvent.IsXML = text.Contains(Tags.TagStart[0].Trim());
							if (text.Substring(0, 1) == "*")
							{
								flag2 = false;
							}
							else if (text.Contains(Tags.TagStart[0].Trim()))
							{
								flag2 = true;
								LinesOfAnEvent = new LinesForAnEvent();
							}
							flag3 = false;
						}
						if (!flag2)
						{
							if (text.Substring(0, 1) == "*")
							{
								LinesOfAnEvent = new LinesForAnEvent();
								LinesOfAnEvent.IsXML = false;
								if (!int.TryParse(text.Substring(21, 4), out var result5))
								{
									result5 = 0;
								}
								if (!int.TryParse(text.Substring(25, 2), out var result6))
								{
									result6 = 0;
								}
								if (!int.TryParse(text.Substring(27, 2), out var result7))
								{
									result7 = 0;
								}
								if (!double.TryParse(text.Substring(65, 6), out var result8))
								{
									result8 = 0.0;
								}
								LinesOfAnEvent.EventJD = Utilities.JD_from_Date(result5, result6, (double)result7 + result8 / 24.0);
								string text8 = " ";
								int.TryParse(text.Substring(1, 6), out var result9);
								LinesOfAnEvent.AstNum = result9;
								if (result9 > 0 && DisplayMPOccultations.IsInDAMIT(result9))
								{
									text8 = "#";
								}
								text2 = text.Substring(21, 4) + " " + text.Substring(25, 2) + " " + text.Substring(27, 2) + text8 + text.Substring(1, 20);
							}
							if (text.Substring(0, 1) == "^")
							{
								if (!int.TryParse(text.Substring(61, 4), out result))
								{
									result = 0;
								}
								if (!int.TryParse(text.Substring(66, 2), out result2))
								{
									result2 = 0;
								}
								if (!double.TryParse(text.Substring(69, 10), out result3))
								{
									result3 = 0.0;
								}
								LinesOfAnEvent.EventReferenceJD = Utilities.JD_from_Date(result, result2, result3);
							}
							if ("*#^".IndexOf(text.Substring(0, 1)) < 0)
							{
								num++;
								if (text.Substring(83, 1) == "D")
								{
									num2++;
								}
							}
							LinesOfAnEvent.Lines.Add(text);
						}
						else
						{
							if (text.Contains(Tags.TagStart[1]))
							{
								LinesOfAnEvent.IsXML = true;
								Parse_EventDate(text, out var Year, out var Month, out var Day, out var MidT);
								LinesOfAnEvent.EventJD = Utilities.JD_from_Date(Year, Month, (double)Day + MidT / 24.0);
								text2 = Year.ToString().PadLeft(4) + Month.ToString().PadLeft(3) + Day.ToString().PadLeft(3);
							}
							if (text.Contains(Tags.TagStart[3]))
							{
								string[] array = text.Trim().Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "")
									.Split(new char[1] { '|' });
								LinesOfAnEvent.StarID = array[0] + " " + array[1];
							}
							if (text.Contains(Tags.TagStart[4]))
							{
								string[] array2 = text.Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
									.Split(new char[1] { '|' });
								string text9 = " ";
								int result10 = 0;
								int.TryParse(array2[0].PadLeft(1), out result10);
								LinesOfAnEvent.AstNum = result10;
								LinesOfAnEvent.AstName = array2[1];
								if (DisplayMPOccultations.IsInDAMIT(result10) | DisplayMPOccultations.IsInISAM(result10))
								{
									text9 = "#";
								}
								DisplayMPOccultations.IsInISAM(result10, out var NumModels);
								DisplayMPOccultations.IsInDAMIT(result10, out var NumModels2);
								num3 = NumModels2 + NumModels;
								text2 = text2 + text9 + array2[0].PadLeft(7) + " " + Utilities.HTML_DecodeString(array2[1]).PadRight(13).Substring(0, 13);
							}
							else if (text.Contains(Tags.TagStart[12]))
							{
								int Day2 = 0;
								Parse_ReferenceTime(text, out result, out result2, out Day2, out Hour, out var _, out var _);
								LinesOfAnEvent.EventReferenceJD = Utilities.JD_from_Date(result, result2, (double)Day2 + Hour / 24.0);
							}
							if (text.Contains(Tags.TagStart[9]))
							{
								num4++;
							}
							if (text.Contains(Tags.TagStart[20]))
							{
								num++;
							}
							if (text.Contains(Tags.TagStart[23]) && text.Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
								.Split(new char[1] { '|' })[1] == "D")
							{
								num2++;
							}
							LinesOfAnEvent.Lines.Add(text);
						}
						flag = true;
					}
				}
				while (!streamReader.EndOfStream);
				if (flag)
				{
					if (num3 > 0 && num4 < num3)
					{
						text2 = text2.Replace("#", "†");
					}
					LinesOfAnEvent.IndexLine = text2 + num.ToString().PadLeft(4) + " " + num2.ToString().PadLeft(3);
					LinesOfAnEvent.ChordCount = num2;
					LinesOfAnEvent.ObsFileisCurrentVersion = obsFileisCurrentVersion;
					LinesOfAnEvent.CurrentFileVersion = text3;
					OccEvents.Add(LinesOfAnEvent);
				}
			}
			OccEvents.Sort();
			if (Data_and_Plots.Observations_Editor != null)
			{
				Data_and_Plots.Observations_Editor.DisplayIndex(ForCurrentEvent: false);
			}
		}

		internal void WriteHistoricalObservationsFile(bool HistoricalFile, string SingleFileName)
		{
			string text = Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile;
			if (!HistoricalFile)
			{
				text = SingleFileName;
			}
			else if (File.Exists(text) && DateTime.UtcNow.Subtract(File.GetLastWriteTimeUtc(text)).TotalHours > 24.0)
			{
				FileInfo fileInfo = new FileInfo(text);
				string text2 = Path.GetFileNameWithoutExtension(text) + "_pre" + DateTime.UtcNow.ToShortDateString().Replace("/", "") + fileInfo.Extension;
				while (File.Exists(fileInfo.Directory!.FullName + "\\" + text2))
				{
					text2 += "x";
				}
				fileInfo.MoveTo(fileInfo.Directory!.FullName + "\\" + text2);
			}
			if (text == "")
			{
				string text3 = Tags.TagFileStart + "\r\n";
				text3 = text3 + "  <FileVersion>" + Tags.ObservationsFileVersion + "</FileVersion>\r\n";
				for (int i = 0; i < OccEvents.Count; i++)
				{
					for (int j = 0; j < OccEvents[i].Lines.Count; j++)
					{
						text3 = text3 + OccEvents[i].Lines[j] + "\r\n";
					}
				}
				text3 = text3 + Tags.TagFileEnd + "\r\n";
				Clipboard.SetText(text3);
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(text);
			streamWriter.WriteLine(Tags.TagFileStart);
			streamWriter.WriteLine("  <FileVersion>" + Tags.ObservationsFileVersion + "</FileVersion>");
			for (int k = 0; k < OccEvents.Count; k++)
			{
				for (int l = 0; l < OccEvents[k].Lines.Count; l++)
				{
					streamWriter.WriteLine(OccEvents[k].Lines[l]);
				}
			}
			streamWriter.WriteLine(Tags.TagFileEnd);
		}

		internal void DecodeAnEvent_Into_EventDetails(int EventNumber)
		{
			int MissNum = 0;
			int PrimaryChords = 0;
			DecodeAnEvent_Into_EventDetails(EventNumber, out MissNum, out PrimaryChords);
		}

		internal void DecodeAnEvent_Into_EventDetails(int EventNumber, out int MissNum, out int PrimaryChords)
		{
			MissNum = 0;
			PrimaryChords = 0;
			EventDetails.Doubles.Clear();
			EventDetails.Satellites.Clear();
			EventDetails.ShapeData.Clear();
			EventDetails.Observers.Clear();
			EventDetails.StarIsDouble = (EventDetails.AsteroidHasSatellite = false);
			EventDetails.NumberOfDoubleSolutions = 0;
			EventDetails.NumberOfSatellites = 0;
			EventDetails.ShapeModelFitted = false;
			bool centerStarPlotOnPlot;
			Data_and_Plots.PlotForm.drawStarImageAtPlotCenterToolStripMenuItem.set_Checked(centerStarPlotOnPlot = false);
			Data_and_Plots.CenterStarPlotOnPlot = centerStarPlotOnPlot;
			for (int i = 0; i < 4; i++)
			{
				DoubleData doubleData = new DoubleData();
				doubleData.Companion_Set = false;
				double num2 = (doubleData.Offset_Y = 0.0);
				double num4 = (doubleData.Offset_X = num2);
				double num6 = (doubleData.Centre_Y = num4);
				double num8 = (doubleData.Centre_X = num6);
				double num11 = (doubleData.Sep_Companion = (doubleData.PA_Companion = num8));
				EventDetails.Doubles.Add(doubleData);
			}
			for (int j = 0; j < 4; j++)
			{
				SatelliteData item = new SatelliteData();
				EventDetails.Satellites.Add(item);
			}
			if (OccEvents[EventNumber].Lines[0].Substring(0, 1) == "*")
			{
				EventDetails.SourceFileIsXML = false;
				for (int k = 3; k < OccEvents[EventNumber].Lines.Count; k++)
				{
					string line = OccEvents[EventNumber].Lines[k];
					ObserverData.Prediction = new ObserverData();
					ObserverData.Prediction.DecodeOldObserverLine(line);
					if (ObserverData.Prediction.PlotCode != "x")
					{
						if ("drm".Contains(ObserverData.Prediction.Event_D) | "drm".Contains(ObserverData.Prediction.Event_R))
						{
							EventDetails.StarIsDouble = true;
						}
						if ("GBgb".Contains(ObserverData.Prediction.Event_D) | "GBgb".Contains(ObserverData.Prediction.Event_R))
						{
							EventDetails.AsteroidHasSatellite = true;
						}
						if ((ObserverData.Prediction.Event_D == "M") | (ObserverData.Prediction.Event_R == "M"))
						{
							MissNum++;
						}
						if ((ObserverData.Prediction.Event_D == "D") | (ObserverData.Prediction.Event_R == "R"))
						{
							PrimaryChords++;
						}
					}
					EventDetails.Observers.Add(ObserverData.Prediction);
				}
				string text = OccEvents[EventNumber].Lines[0];
				string text2 = OccEvents[EventNumber].Lines[1];
				string text3 = OccEvents[EventNumber].Lines[2];
				EventDetails.AsteroidNumber = text.Substring(1, 6).Trim();
				EventDetails.AsteroidID = text.Substring(8, 13).Trim();
				if (!int.TryParse(text.Substring(21, 4), out var result))
				{
					result = 0;
				}
				EventDetails.Year = result;
				if (!int.TryParse(text.Substring(25, 2), out result))
				{
					result = 0;
				}
				EventDetails.Month = result;
				if (!int.TryParse(text.Substring(27, 2), out result))
				{
					result = 0;
				}
				EventDetails.Day = result;
				string text4 = text.Substring(30, 13);
				if (text4.Contains("HIP"))
				{
					EventDetails.StarCat = "HIP";
					EventDetails.StarNumber = text4.Replace("HIP", "").Trim();
				}
				else if (text4.Contains("4UC"))
				{
					EventDetails.StarCat = "UCAC4";
					EventDetails.StarNumber = text4.Replace("4UC", "").Trim();
				}
				else if (text4.Contains("2U"))
				{
					EventDetails.StarCat = "UCAC2";
					EventDetails.StarNumber = text4.Replace("2U", "").Trim();
				}
				else if (text4.Contains("3UC"))
				{
					EventDetails.StarCat = "UCAC3";
					EventDetails.StarNumber = text4.Replace("3UC", "").Trim();
				}
				else if (text4.Contains("1UT"))
				{
					EventDetails.StarCat = "URAT1";
					EventDetails.StarNumber = text4.Replace("1UT", "").Trim();
				}
				else if (text4.Contains("B"))
				{
					EventDetails.StarCat = "USNO-B1";
					EventDetails.StarNumber = text4.Replace("B", "").Trim();
				}
				else if (text4.Contains("N"))
				{
					EventDetails.StarCat = "NOMAD";
					EventDetails.StarNumber = text4.Replace("N", "").Trim();
				}
				else
				{
					EventDetails.StarCat = "Tycho2";
					EventDetails.StarNumber = text4.Trim();
				}
				if (!int.TryParse(text.Substring(43, 2), out var result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(text.Substring(45, 2), out var result3))
				{
					result3 = 0;
				}
				if (!double.TryParse(text.Substring(47, 7), out var result4))
				{
					result4 = 0.0;
				}
				if (!int.TryParse(text.Substring(137, 1), out var result5))
				{
					result5 = 0;
				}
				double num12 = result4 + (double)result5 / 100000.0;
				EventDetails.RA_Star_2000 = ((double)result2 + (double)result3 / 60.0 + num12 / 3600.0) * 15.0 / (180.0 / Math.PI);
				if (!int.TryParse(text.Substring(55, 2), out result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(text.Substring(57, 2), out result3))
				{
					result3 = 0;
				}
				if (!double.TryParse(text.Substring(59, 6), out result4))
				{
					result4 = 0.0;
				}
				if (!int.TryParse(text.Substring(138, 1), out result5))
				{
					result5 = 0;
				}
				num12 = result4 + (double)result5 / 10000.0;
				EventDetails.Dec_Star_2000 = ((double)result2 + (double)result3 / 60.0 + num12 / 3600.0) / (180.0 / Math.PI);
				if (text.Substring(54, 1) == "-")
				{
					EventDetails.Dec_Star_2000 = 0.0 - EventDetails.Dec_Star_2000;
				}
				Utilities.ApparentStarPosition(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDetails.JD_EventDate, use2006values_Not1976: false, out var RA_end, out var Dec_end);
				EventDetails.RA_Star_Apparent = RA_end;
				EventDetails.Dec_Star_Apparent = Dec_end;
				if (!double.TryParse(text.Substring(65, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.MidT_forMotions = result4;
				Application.DoEvents();
				if (!double.TryParse(text.Substring(71, 8), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.dRA = result4;
				if (!double.TryParse(text.Substring(79, 8), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.d2RA = result4;
				if (!double.TryParse(text.Substring(87, 7), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.dDec = result4;
				if (!double.TryParse(text.Substring(94, 7), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.d2Dec = result4;
				if (!double.TryParse(text.Substring(101, 7), out result4))
				{
					result4 = 1.0;
				}
				EventDetails.Parallax = result4;
				EventDetails.dParallax = 0.0;
				int.TryParse(text.Substring(126, 1), out result3);
				EventDetails.GaiaVersion = result3;
				if (!int.TryParse(text.Substring(127, 4), out result))
				{
					result = 0;
				}
				EventDetails.YearAdded = result;
				if (!int.TryParse(text.Substring(131, 3), out result))
				{
					result = 0;
				}
				EventDetails.MonthAdded = result;
				if (!int.TryParse(text.Substring(134, 3), out result))
				{
					result = 0;
				}
				EventDetails.DayAdded = result;
				if (!int.TryParse(text.Substring(139, 4), out result))
				{
					result = 0;
				}
				EventDetails.YearEdited = result;
				if (!int.TryParse(text.Substring(143, 3), out result))
				{
					result = 0;
				}
				EventDetails.MonthEdited = result;
				if (!int.TryParse(text.Substring(146, 3), out result))
				{
					result = 0;
				}
				EventDetails.DayEdited = result;
				if (!double.TryParse(text2.Substring(1, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.MgStar = result4;
				if (!double.TryParse(text2.Substring(7, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.MvAsteroid = result4;
				if (!int.TryParse(text2.Substring(13, 5), out result))
				{
					result = 0;
				}
				EventDetails.AsteroidNominalDiameter = result;
				int.TryParse(EventDetails.AsteroidNumber, out var result6);
				double num13 = 0.0;
				if (result6 < 550000 && result6 > 0)
				{
					using FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin", FileMode.Open, FileAccess.Read);
					using BinaryReader binaryReader = new BinaryReader(fileStream);
					fileStream.Seek(2 * (result6 - 1), SeekOrigin.Begin);
					num13 = (double)binaryReader.ReadInt16() / 10.0;
				}
				if (num13 > 0.0)
				{
					EventDetails.AsteroidNominalDiameter = num13;
				}
				if (!double.TryParse(text2.Substring(19, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.X = result4;
				if (!double.TryParse(text2.Substring(25, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Y = result4;
				if (!double.TryParse(text2.Substring(31, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.X_Dia = result4;
				if (!double.TryParse(text2.Substring(37, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Y_Dia = result4;
				if (!double.TryParse(text2.Substring(43, 6), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.PA_Ellipse = result4;
				if (EventDetails.StarIsDouble)
				{
					DoubleData doubleData2 = new DoubleData();
					doubleData2.Companion_Set = double.TryParse(text2.Substring(49, 6), out result4);
					doubleData2.PA_Companion = result4;
					double.TryParse(text2.Substring(55, 6), out result4);
					doubleData2.Sep_Companion = result4;
					double.TryParse(text2.Substring(89, 6), out result4);
					doubleData2.Sdev_PA_Companion = result4;
					double.TryParse(text2.Substring(83, 6), out result4);
					doubleData2.Sdev_Sep_Companion = result4;
					double num6 = (doubleData2.Offset_Y = 0.0);
					double num8 = (doubleData2.Offset_X = num6);
					double num11 = (doubleData2.Centre_X = (doubleData2.Centre_Y = num8));
					doubleData2.SolutionID = 1;
					EventDetails.Doubles[0] = doubleData2;
					EventDetails.NumberOfDoubleSolutions = 1;
					EventDetails.JDSO_SubmitDate = (EventDetails.JDSO_Vol_Num_Pg = "");
				}
				if (EventDetails.AsteroidHasSatellite)
				{
					SatelliteData satelliteData = new SatelliteData();
					satelliteData.CompanionIAUname = "ID to be set";
					double num6 = (satelliteData.Sat_d2Dec_mas = 0.0);
					double num8 = (satelliteData.Sat_d2RA_mas = num6);
					double num11 = (satelliteData.Sat_dRA_mas = (satelliteData.Sat_dDec_mas = num8));
					double.TryParse(text2.Substring(49, 6), out result4);
					satelliteData.SatellitePA_2000 = result4;
					double.TryParse(text2.Substring(55, 6), out result4);
					satelliteData.SatelliteSeparation = result4;
					num11 = (satelliteData.Sat_Sep_Uncertainty = (satelliteData.Sat_PA_Uncertainty = 0.0));
					if (!double.TryParse(text.Substring(108, 6), out result4))
					{
						result4 = 0.0;
					}
					satelliteData.MajorAxisSatellite = result4;
					if (!double.TryParse(text.Substring(114, 6), out result4))
					{
						result4 = 0.0;
					}
					satelliteData.MinorAxisSatellite = result4;
					if (!double.TryParse(text.Substring(120, 6), out result4))
					{
						result4 = 0.0;
					}
					satelliteData.PAAxisSatellite = result4;
					double.TryParse(text2.Substring(49, 6), out result4);
					satelliteData.SatellitePA_Apparent = result4;
					double.TryParse(text2.Substring(55, 6), out result4);
					satelliteData.SatelliteSeparation = result4;
					satelliteData.NumberOfChords = 1;
					EventDetails.Satellites[0] = satelliteData;
				}
				if (!int.TryParse(text2.Substring(63, 1), out result))
				{
					result = 0;
				}
				EventDetails.Quality = result;
				EventDetails.FlagForReview = 0;
				EventDetails.Sdev_Major_Set = double.TryParse(text2.Substring(65, 6), out result4);
				EventDetails.Sdev_Major = result4;
				EventDetails.Sdev_Minor_Set = double.TryParse(text2.Substring(71, 6), out result4);
				EventDetails.Sdev_Minor = result4;
				EventDetails.Sdev_PA_Ellipse_Set = double.TryParse(text2.Substring(77, 6), out result4);
				EventDetails.Sdev_PA_Ellipse = result4;
				EventDetails.Sdev_Sep_Set = double.TryParse(text2.Substring(83, 6), out result4);
				EventDetails.Sdev_Sep = result4;
				EventDetails.Sdev_PA_Star_Set = double.TryParse(text2.Substring(89, 6), out result4);
				EventDetails.Sdev_PA_Star = result4;
				EventDetails.Sdev_X_Set = double.TryParse(text2.Substring(95, 6), out result4);
				EventDetails.Sdev_X = result4;
				EventDetails.Sdev_Y_Set = double.TryParse(text2.Substring(101, 6), out result4);
				EventDetails.Sdev_Y = result4;
				if (!long.TryParse(text2.Substring(110, 9), out var result7))
				{
					result7 = 0L;
				}
				EventDetails.Inc_Miss = text2.Substring(119, 1) == "1";
				EventDetails.Solve_X = true;
				EventDetails.Solve_X = true;
				EventDetails.Solve_Major = text2.Substring(120, 1) == "1";
				EventDetails.Solve_Minor = text2.Substring(121, 1) == "1";
				EventDetails.Solve_PA = text2.Substring(122, 1) == "1";
				EventDetails.Solve_CompanionSep = text2.Substring(123, 1) == "1";
				EventDetails.Solve_CompanionPA = text2.Substring(124, 1) == "1";
				EventDetails.Solve_Circular = text2.Substring(125, 1) == "1";
				EventDetails.MPCDate = text2.Substring(134, 8);
				EventDetails.MPCNumber = text2.Substring(142, 7);
				if (!double.TryParse(text3.Substring(1, 11), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.RefHour_forAnalysis = result4;
				if (!double.TryParse(text3.Substring(12, 9), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.X_Geo_atEvent = result4;
				if (!double.TryParse(text3.Substring(21, 8), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Y_Geo_atEvent = result4;
				if (!double.TryParse(text3.Substring(29, 10), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.X_Geo_atConj = result4;
				if (!double.TryParse(text3.Substring(39, 8), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Y_Geo_atConj = result4;
				if (!double.TryParse(text3.Substring(47, 8), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Sep_km_atConj = result4;
				if (!int.TryParse(text3.Substring(61, 4), out result))
				{
					result = 0;
				}
				EventDetails.Year_Conj = result;
				if (!int.TryParse(text3.Substring(66, 2), out result))
				{
					result = 0;
				}
				EventDetails.Month_Conj = result;
				if (!double.TryParse(text3.Substring(69, 10), out result4))
				{
					result4 = 0.0;
				}
				EventDetails.Day_Conj = result4;
				double.TryParse(text3.Substring(85, 8), out result4);
				EventDetails.Sep_Conj = result4;
				double.TryParse(text3.Substring(98, 8), out result4);
				EventDetails.PA_Conj_2000 = result4;
				double.TryParse(text3.Substring(113, 6), out result4);
				EventDetails.Sdev_Sep_Conj = result4;
				double.TryParse(text3.Substring(126, 9), out result4);
				EventDetails.Sdev_T_Conj = result4;
				if (!int.TryParse(text3.Substring(140, 4), out result))
				{
					result = 0;
				}
				EventDetails.Number_Chords = result;
				EventDetails.AstrometryShapeModelCentered = false;
				EventDetails.UsedAssumedDiameter = OccEvents[EventNumber].ChordCount < 2;
				return;
			}
			EventDetails.MPCDate = (EventDetails.MPCNumber = (EventDetails.MPCsubmissionID = ""));
			EventDetails.SourceFileIsXML = true;
			int SolutionCount = 0;
			EventDetails.JDSO_SubmitDate = (EventDetails.JDSO_Vol_Num_Pg = "");
			for (int l = 0; l < 4; l++)
			{
				double num11 = (EventDetails.Doubles[l].Sep_Companion = (EventDetails.Doubles[l].PA_Companion = 0.0));
			}
			EventDetails.StarIsDouble = false;
			EventDetails.StarReliability = 1.0;
			EventDetails.GaiaPMfromUCAC4 = 0;
			EventDetails.NoGaiaPM = 0;
			EventDetails.AsteroidHasSatellite = false;
			EventDetails.NumberOfSatellites = 0;
			SatelliteData item2 = new SatelliteData();
			EventDetails.Satellites.Clear();
			for (int m = 0; m < 4; m++)
			{
				EventDetails.Satellites.Add(item2);
			}
			EventDetails.RefHour_forAnalysis = 0.0;
			int num26 = 0;
			int num27 = 0;
			for (int n = 0; n < OccEvents[EventNumber].Lines.Count; n++)
			{
				if (OccEvents[EventNumber].Lines[n].Contains("<FileVersion>"))
				{
					double.TryParse(OccEvents[EventNumber].Lines[n].Replace("<FileVersion>", "").Replace("</FileVersion>", "").Trim(), out VersionOfReadFile);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[1]))
				{
					Parse_EventDate(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[3]))
				{
					Parse_Star(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[34]))
				{
					Parse_StarIssues(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[4]))
				{
					Parse_Asteroid(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[5]))
				{
					Parse_SolveFlags(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[6]))
				{
					Parse_EllipticFit(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[7]))
				{
					Parse_EllipticUncertainty(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[9]))
				{
					Parse_ShapeModelFit(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[10]))
				{
					Parse_Satellite(OccEvents[EventNumber].Lines[n], num26);
					num26++;
					EventDetails.AsteroidHasSatellite = true;
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[12]))
				{
					Parse_ReferenceTime(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[13]))
				{
					Parse_MainBodyLine(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart_Old[0]))
				{
					Parse_MainBodyLine(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[14]))
				{
					Parse_SecondaryLine(OccEvents[EventNumber].Lines[n], num27);
					num27++;
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[31]))
				{
					Parse_SecondaryAtConjunction(OccEvents[EventNumber].Lines[n], num27 - 1);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[15]))
				{
					Parse_MPC(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[35]))
				{
					Parse_JDSO(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[17]))
				{
					Parse_DoubleStarSolution(OccEvents[EventNumber].Lines[n], ref SolutionCount);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[19]))
				{
					Parse_EventPrediction(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[20]))
				{
					Parse_Observer(OccEvents[EventNumber].Lines[n + 1], OccEvents[EventNumber].Lines[n + 2], OccEvents[EventNumber].Lines[n + 3], OccEvents[EventNumber].Lines[n + 4]);
					n += 4;
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[25]))
				{
					Parse_Added(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[26]))
				{
					Parse_LastEdited(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[29]))
				{
					Parse_AtConjunction(OccEvents[EventNumber].Lines[n]);
				}
				else if (OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart_Old[1]))
				{
					Parse_AtConjunction(OccEvents[EventNumber].Lines[n]);
				}
			}
			if (EventDetails.ShapeData.Count == 0)
			{
				EventDetails.AstrometryShapeModelCentered = false;
			}
			if ((EventDetails.X_Dia == 0.0001) | (EventDetails.Y_Dia == 0.0001))
			{
				EventDetails.UsedAssumedDiameter = true;
				EventDetails.AsteroidNominalDiameter = (EventDetails.X_Dia = (EventDetails.Y_Dia = Data_and_Plots.GetAssumedDiameter()));
			}
			try
			{
				Data_and_Plots.PlotForm.showStarWithADiameterOfToolStripMenuItem.set_Checked(true);
			}
			catch
			{
			}
		}

		internal bool Paste_An_Event()
		{
			string[] array = Clipboard.GetText().Replace("\r", "").Replace("\t", "")
				.Split(new char[1] { '\n' });
			if (array.GetUpperBound(0) < 2)
			{
				return false;
			}
			Data_and_Plots.SingleEvent.OccEvents.Clear();
			LinesForAnEvent linesForAnEvent = new LinesForAnEvent();
			for (int i = 0; i <= array.GetUpperBound(0); i++)
			{
				if (!array[i].Contains("Observations>") & (array[i].Trim().Length > 0))
				{
					linesForAnEvent.Lines.Add(array[i]);
				}
			}
			Data_and_Plots.SingleEvent.OccEvents.Add(linesForAnEvent);
			return true;
		}

		internal bool Paste_Observers()
		{
			string[] array = Clipboard.GetText((TextDataFormat)0).Replace("\r", "").Split(new char[1] { '\n' });
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Contains(Tags.TagStart[20]) & (array.Length > i + 4))
				{
					Data_and_Plots.SingleEvent.Parse_Observer(array[i + 1], array[i + 2], array[i + 3], array[i + 4]);
				}
			}
			return true;
		}

		internal void GetMainObserverLocation(int EventNumber, out double Longitude, out double Latitude, out bool StarIsDouble, out bool AsteroidIsDouble, out string EventID, out double AsteroidDiameter, out double MagDrop, out int NumChords)
		{
			Longitude = (Latitude = (AsteroidDiameter = (MagDrop = 0.0)));
			NumChords = 0;
			StarIsDouble = (AsteroidIsDouble = false);
			EventID = "";
			double result = 20.0;
			double result2 = 20.0;
			if (!int.TryParse(OccEvents[EventNumber].IndexLine.Substring(37, 3), out NumChords))
			{
				NumChords = 0;
			}
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[1]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						EventID = array[0] + " " + Utilities.ShortMonths[int.Parse(array[1])] + array[2].PadLeft(2, '0').PadLeft(3);
					}
					else if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[3]))
					{
						double.TryParse(OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "")
							.Split(new char[1] { '|' })[13], out result);
					}
					else if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
					{
						string[] array2 = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						EventID = EventID + " : " + ("(" + array2[0] + ") ").PadLeft(9) + array2[1];
						double.TryParse(array2[10], out AsteroidDiameter);
						double.TryParse(array2[12], out result2);
					}
					else if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[21]))
					{
						string[] array3 = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
							.Split(new char[1] { '|' });
						double.TryParse(array3[6].Substring(0, 4).Replace("-", "").Replace("+", ""), out Longitude);
						if (array3[6].Substring(0, 4).Contains("-"))
						{
							Longitude = 0.0 - Longitude;
						}
						double.TryParse(array3[7].Substring(0, 3).Replace("-", "").Replace("+", ""), out Latitude);
						if (array3[7].Substring(0, 3).Contains("-"))
						{
							Latitude = 0.0 - Latitude;
						}
						if (!((Longitude == 0.0) & (Latitude == 0.0)))
						{
							break;
						}
					}
				}
				MagDrop = result2 - Utilities.CombinedMagnitude(result2, result);
				for (int j = 0; j < OccEvents[EventNumber].Lines.Count; j++)
				{
					if (OccEvents[EventNumber].Lines[j].Contains(Tags.TagStart[16]))
					{
						StarIsDouble = true;
						break;
					}
				}
				for (int k = 0; k < OccEvents[EventNumber].Lines.Count; k++)
				{
					if (OccEvents[EventNumber].Lines[k].Contains(Tags.TagStart[10]))
					{
						AsteroidIsDouble = true;
						break;
					}
				}
				return;
			}
			double.TryParse(OccEvents[EventNumber].Lines[1].Substring(55, 6), out var result3);
			StarIsDouble = result3 > 0.0;
			for (int l = 3; l < OccEvents[EventNumber].Lines.Count; l++)
			{
				string text = OccEvents[EventNumber].Lines[l];
				if (!text.Substring(3, 37).ToUpper().Contains("PREDICT"))
				{
					Longitude = double.Parse(text.Substring(41, 4).Replace('-', ' ').Replace('+', ' '));
					if (text.Substring(41, 4).Contains("-"))
					{
						Longitude = 0.0 - Longitude;
					}
					Latitude = double.Parse(text.Substring(52, 3).Replace('-', ' ').Replace('+', ' '));
					if (text.Substring(52, 3).Contains("-"))
					{
						Latitude = 0.0 - Latitude;
					}
					if (!((Longitude == 0.0) & (Latitude == 0.0)))
					{
						break;
					}
				}
			}
		}

		internal bool CheckEventForFormatErrors(int EventNumber, bool IncludeTelescopeApertureInCheck, out string ErrorList)
		{
			ErrorList = "";
			string text = "";
			string text2 = "";
			if (OccEvents[EventNumber].IsXML)
			{
				StringBuilder stringBuilder = new StringBuilder();
				text = "";
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[6]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
							.Split(new char[1] { '|' });
						double.TryParse(array[2], out var result);
						double.TryParse(array[3], out var result2);
						if (result <= 0.0)
						{
							text2 = text2 + "Xdia (" + array[2] + "km) ";
						}
						if (result2 <= 0.0)
						{
							text2 = text2 + "Ydia (" + array[3] + "km) ";
						}
					}
					else
					{
						if (!OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[21]))
						{
							continue;
						}
						text2 = "";
						string[] array2 = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
							.Split(new char[1] { '|' });
						string[] array3 = OccEvents[EventNumber].Lines[i + 2].Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
							.Split(new char[1] { '|' });
						string[] array4 = OccEvents[EventNumber].Lines[i + 3].Trim().Replace(Tags.TagStart[24], "").Replace(Tags.TagEnd[24], "")
							.Split(new char[1] { '|' });
						double.TryParse(array2[6].Substring(0, 4).Replace("-", "").Replace("+", ""), out var result3);
						double.TryParse(array2[6].Substring(5, 2).Replace("-", "").Replace("+", ""), out var result4);
						double.TryParse(array2[6].Substring(8).Replace("-", "").Replace("+", ""), out var result5);
						result3 += result4 / 60.0 + result5 / 3600.0;
						if (array2[6].Substring(0, 4).Contains("-"))
						{
							result3 = 0.0 - result3;
						}
						double.TryParse(array2[7].Substring(0, 3).Replace("-", "").Replace("+", ""), out var result6);
						double.TryParse(array2[6].Substring(5, 2).Replace("-", "").Replace("+", ""), out var result7);
						double.TryParse(array2[6].Substring(8).Replace("-", "").Replace("+", ""), out var result8);
						result6 += result7 / 60.0 + result8 / 3600.0;
						if (array2[7].Substring(0, 3).Contains("-"))
						{
							result6 = 0.0 - result6;
						}
						double.TryParse(array2[8], out var result9);
						if (((result3 <= -180.0) | ((result3 == 0.0) & (Math.Abs(result6) != 90.0))) || result3 >= 360.0)
						{
							text2 += "Long ";
						}
						if (result6 < -90.0 || result6 == 0.0 || result6 > 90.0)
						{
							text2 += "Lat ";
						}
						if (result9 < -300.0 || result9 > 5800.0)
						{
							text2 = text2 + "Alt (" + array2[8] + "m) ";
						}
						if (array3[0] == array4[0] && ((array3[1].ToUpper() != "M") & (array3[1] != "C") & (array4[1] != "C") & (array3[1] != "e") & (array4[1] != "f")))
						{
							text2 = text2 + "D=R (" + array3[0] + ") ";
						}
						double.TryParse(array3[2], out var result10);
						double.TryParse(array4[2], out var result11);
						if (result10 < 0.0 || result10 > 10.0)
						{
							text2 = text2 + "AccD (" + array3[2] + "s) ";
						}
						if (result11 < 0.0 || result11 > 10.0)
						{
							text2 = text2 + "AccR (" + array4[2] + "s) ";
						}
						if (IncludeTelescopeApertureInCheck)
						{
							double.TryParse(array2[10], out var result12);
							if (result12 < 0.0 || result12 > 110.0)
							{
								text2 = text2 + "TelAp (" + array2[10] + "cm) ";
							}
						}
						if (text2.Length > 0)
						{
							stringBuilder.AppendLine(array2[0].PadRight(3) + array2[1].PadRight(12).Substring(0, 12) + ": " + text2);
						}
					}
				}
				stringBuilder.AppendLine("");
				ErrorList = GetEventID(EventNumber, IncludeMagnitude: true, Kepler2: false) + "\r\n" + stringBuilder.ToString() + "".PadRight(50, '*');
				return stringBuilder.Length > 10;
			}
			StringBuilder stringBuilder2 = new StringBuilder();
			string text3 = OccEvents[EventNumber].Lines[1];
			text = "";
			if (double.TryParse(text3.Substring(31, 6), out var result13) && result13 == 0.0)
			{
				text += "Xdia ";
			}
			if (double.TryParse(text3.Substring(37, 6), out var result14) && result14 == 0.0)
			{
				text += "Ydia ";
			}
			if (text.Length > 0)
			{
				stringBuilder2.AppendLine(text);
				stringBuilder2.AppendLine(text3);
			}
			int result15 = 0;
			int result16 = 0;
			for (int j = 3; j < OccEvents[EventNumber].Lines.Count; j++)
			{
				string text4 = OccEvents[EventNumber].Lines[j];
				bool flag = text4.Substring(0, 20).Contains("Predict");
				text = "";
				if (!int.TryParse(text4.Substring(41, 4).Replace(" ", ""), out var result17))
				{
					result17 = 180;
				}
				if (!int.TryParse(text4.Substring(45, 2), out var result18))
				{
					result18 = 60;
				}
				if (!double.TryParse(text4.Substring(47, 5), out var result19) && !flag)
				{
					result19 = 60.0;
				}
				if (result17 > 179 || result18 > 59 || result18 < 0 || result19 >= 60.0 || result19 < 0.0)
				{
					text += "Long ";
				}
				if (!int.TryParse(text4.Substring(52, 3).Replace(" ", ""), out result15))
				{
					result15 = 90;
				}
				if (!int.TryParse(text4.Substring(55, 2), out result16))
				{
					result16 = 60;
				}
				if (!double.TryParse(text4.Substring(57, 4), out var result20) && !flag)
				{
					result20 = 60.0;
				}
				if (result15 > 80 || result16 > 59 || result16 < 0 || result20 >= 60.0 || result20 < 0.0)
				{
					text += "Lat ";
				}
				if (!int.TryParse(text4.Substring(61, 5), out var result21))
				{
					result21 = 0;
				}
				if ((result21 < -300 || result21 > 5800) && !flag)
				{
					text += "Alt ";
				}
				if (!flag)
				{
					if (!int.TryParse(text4.Substring(74, 2), out var result22) & (text4.Substring(74, 2).Trim().Length > 0))
					{
						result22 = 25;
					}
					if (!int.TryParse(text4.Substring(76, 2), out var result23) & (text4.Substring(76, 2).Trim().Length > 0))
					{
						result23 = 60;
					}
					if (!double.TryParse(text4.Substring(78, 5), out var result24) & (text4.Substring(78, 5).Trim() != "."))
					{
						result24 = 60.0;
					}
					if (result22 < 0 || result22 > 24 || result23 < 0 || result23 > 59 || result24 < 0.0 || result24 >= 60.0)
					{
						text += "Td ";
					}
					if (!int.TryParse(text4.Substring(93, 2), out var result25) & (text4.Substring(93, 2).Trim().Length > 0))
					{
						result25 = 25;
					}
					if (!int.TryParse(text4.Substring(95, 2), out var result26) & (text4.Substring(95, 2).Trim().Length > 0))
					{
						result26 = 60;
					}
					if (!double.TryParse(text4.Substring(97, 5), out var result27) & (text4.Substring(97, 5).Trim() != "."))
					{
						result27 = 60.0;
					}
					if (result25 < 0 || result25 > 24 || result26 < 0 || result26 > 59 || result27 < 0.0 || result27 >= 60.0)
					{
						text += "Tr ";
					}
					if (IncludeTelescopeApertureInCheck && double.TryParse(text4.Substring(67, 3), out var result28) && (result28 <= 0.0 || result28 > 110.0))
					{
						text += "TelAp ";
					}
					bool flag2 = false;
					bool flag3 = false;
					if (double.TryParse(text4.Substring(84, 4), out var result29))
					{
						flag2 = result29 < 0.0 || result29 > 10.0;
					}
					if (double.TryParse(text4.Substring(103, 4), out var result30))
					{
						flag3 = result30 < 0.0 || result30 > 10.0;
					}
					if (flag2 || flag3)
					{
						text += "Acc ";
					}
				}
				if (text.Length > 0)
				{
					stringBuilder2.AppendLine(text);
					stringBuilder2.AppendLine(text4);
				}
			}
			stringBuilder2.AppendLine("");
			ErrorList = GetEventID(EventNumber, IncludeMagnitude: true, Kepler2: false) + "\r\n" + stringBuilder2.ToString() + "".PadRight(50, '*');
			return stringBuilder2.Length > 10;
		}

		internal void GetStarCoords(int EventNumber, out double StarRA, out double StarDec, out string SourceCode)
		{
			GetStarCoords(EventNumber, out StarRA, out StarDec, out SourceCode, out var _);
		}

		internal void GetStarCoords(int EventNumber, out double StarRA, out double StarDec, out string SourceCode, out double StarMag)
		{
			GetStarCoords(EventNumber, out StarRA, out StarDec, out SourceCode, out StarMag, out var _, out var _, out var _);
		}

		internal void GetStarCoords(int EventNumber, out double StarRA, out double StarDec, out string SourceCode, out double StarMag, out string StarCat, out string CatNumber, out double StarDia_mas)
		{
			StarRA = (StarDec = (StarMag = (StarDia_mas = 0.0)));
			SourceCode = (StarCat = (CatNumber = ""));
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[3]))
					{
						Parse_Star(OccEvents[EventNumber].Lines[i], out StarRA, out StarDec, out SourceCode, out StarMag, out StarCat, out CatNumber, out StarDia_mas);
						break;
					}
				}
				return;
			}
			string text = OccEvents[EventNumber].Lines[0];
			try
			{
				StarRA = (double.Parse(text.Substring(43, 2)) + double.Parse(text.Substring(45, 2)) / 60.0 + (double.Parse(text.Substring(47, 7)) + (double)int.Parse(text.Substring(137, 1)) / 100000.0) / 3600.0) * 15.0 / (180.0 / Math.PI);
				StarDec = (double.Parse(text.Substring(55, 2)) + double.Parse(text.Substring(57, 2)) / 60.0 + (double.Parse(text.Substring(59, 6)) + (double)int.Parse(text.Substring(138, 1)) / 10000.0) / 3600.0) / (180.0 / Math.PI);
				if (text.Substring(54, 1) == "-")
				{
					StarDec = 0.0 - StarDec;
				}
			}
			catch
			{
				StarRA = (StarDec = 0.0);
			}
		}

		internal string GetStarID(int EventNumber)
		{
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[3]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "").Split(new char[1] { '|' });
						return array[0].Trim() + " " + array[1];
					}
				}
				return "";
			}
			return OccEvents[EventNumber].Lines[0].Substring(30, 13);
		}

		internal string GetObserverNames(int EventNumber)
		{
			string text = "";
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[21]))
				{
					string[] array = OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "").Split(new char[1] { '|' });
					text = ((text.Length != 0) ? (text + ", " + Utilities.ProperCase(Utilities.HTML_DecodeString(array[1])).Trim().Replace("  ", " ")) : Utilities.ProperCase(Utilities.HTML_DecodeString(array[1])).Trim().Replace("  ", " "));
				}
			}
			return text;
		}

		internal double GetStarRUWE(int EventNumber)
		{
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[34]))
					{
						if (!double.TryParse(OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[34], "").Replace(Tags.TagEnd[34], "").Split(new char[1] { '|' })[0], out var result))
						{
							return -1.0;
						}
						return result;
					}
				}
				return -1.0;
			}
			return -1.0;
		}

		internal string GetFormattedEventDate(int EventNumber)
		{
			GetEventDate(EventNumber, out var Year, out var Month, out var Day);
			return Year + Utilities.ShortMonths[Month].PadLeft(4) + Day.ToString().PadLeft(3);
		}

		internal void GetEventDate(int EventNumber, out int Year, out int Month, out int Day, out double Hour)
		{
			Year = (Month = (Day = 0));
			Hour = 0.0;
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[1]))
				{
					string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
						.Split(new char[1] { '|' });
					int.TryParse(array[0], out Year);
					int.TryParse(array[1], out Month);
					int.TryParse(array[2], out Day);
					double.TryParse(array[3], out Hour);
					break;
				}
			}
		}

		internal void GetEventDate(int EventNumber, out int Year, out int Month, out int Day)
		{
			Year = (Month = (Day = 0));
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[1]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						int.TryParse(array[0], out Year);
						int.TryParse(array[1], out Month);
						int.TryParse(array[2], out Day);
						break;
					}
				}
			}
			else
			{
				string text = OccEvents[EventNumber].Lines[0];
				Year = int.Parse(text.Substring(21, 4));
				Month = int.Parse(text.Substring(25, 2));
				Day = int.Parse(text.Substring(27, 2));
			}
		}

		internal void GetEventDateOfAstrometry(int EventNumber, out int Year, out int Month, out int Day, out double Hour)
		{
			Year = (Month = (Day = 0));
			Hour = 0.0;
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[1]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						int.TryParse(array[0], out Year);
						int.TryParse(array[1], out Month);
						int.TryParse(array[2], out Day);
						double.TryParse(array[3], out Hour);
					}
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagEnd[12]))
					{
						string[] array2 = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[12], "").Replace(Tags.TagEnd[12], "")
							.Split(new char[1] { '|' });
						int.TryParse(array2[0], out Year);
						int.TryParse(array2[1], out Month);
						int.TryParse(array2[2], out Day);
						double.TryParse(array2[3], out Hour);
						break;
					}
				}
			}
			else
			{
				string text = OccEvents[EventNumber].Lines[0];
				Year = int.Parse(text.Substring(21, 4));
				Month = int.Parse(text.Substring(25, 2));
				Day = int.Parse(text.Substring(27, 2));
			}
		}

		internal DateTime GetDate_LastEdited(int EventNumber)
		{
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[26]))
				{
					string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[26], "").Replace(Tags.TagEnd[26], "")
						.Split(new char[1] { '|' });
					int.TryParse(array[0], out var result);
					int.TryParse(array[1], out var result2);
					int.TryParse(array[2], out var result3);
					return new DateTime(result, result2, result3);
				}
			}
			return new DateTime(2100, 1, 1);
		}

		internal double GetStarMag(int EventNumber)
		{
			double result = 0.0;
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[3]))
					{
						double.TryParse(OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "").Split(new char[1] { '|' })[13], out result);
					}
				}
				return result;
			}
			double.TryParse(OccEvents[EventNumber].Lines[1].Substring(2, 5), out result);
			return result;
		}

		internal double GetAsteroidMag(int EventNumber)
		{
			double result = 0.0;
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
					{
						double.TryParse(OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "").Split(new char[1] { '|' })[12], out result);
					}
				}
				return result;
			}
			return 20.0;
		}

		internal double GetMotion_n(int EventNumber)
		{
			double result = 1.0;
			double result2 = 1.0;
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
				{
					string[] array = OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "").Split(new char[1] { '|' });
					double.TryParse(array[2], out result);
					double.TryParse(array[3], out result2);
					break;
				}
			}
			return Math.Sqrt(result * result + result2 * result2);
		}

		internal double GetAsteroidNominalDiameter(int EventNumber)
		{
			double result = -1.0;
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
				{
					double.TryParse(OccEvents[EventNumber].Lines[i].Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "").Split(new char[1] { '|' })[10], out result);
					break;
				}
			}
			return result;
		}

		internal void GetAsteroidID(int EventNumber, out string AsteroidNumber, out string AsteroidName)
		{
			AsteroidNumber = "";
			AsteroidName = "";
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Replace("&apos;", "'")
							.Split(new char[1] { '|' });
						AsteroidNumber = array[0].PadLeft(7);
						AsteroidName = Utilities.HTML_DecodeString(array[1]);
					}
				}
			}
			else
			{
				string text = OccEvents[EventNumber].Lines[0];
				AsteroidNumber = text.Substring(1, 6).PadLeft(7);
				AsteroidName = text.Substring(8, 13).Trim();
			}
		}

		internal bool GetAsteroidSatelliteIDs(int EventNumber, ref string[] SatIDs)
		{
			SatIDs = new string[4];
			int num = 0;
			bool result = false;
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[10]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
							.Split(new char[1] { '|' });
						if (array[0].Contains("="))
						{
							SatIDs[num] = array[0].Substring(0, array[0].IndexOf("=")).Trim();
						}
						else
						{
							SatIDs[num] = array[0].Trim();
						}
						num++;
						result = true;
					}
				}
			}
			return result;
		}

		internal string GetEventID(int EventNumber, bool IncludeMagnitude, bool Kepler2)
		{
			string AsteroidName = "";
			string AsteroidNumber = "";
			string SourceCode = "";
			GetAsteroidID(EventNumber, out AsteroidNumber, out AsteroidName);
			string starID = GetStarID(EventNumber);
			GetStarCoords(EventNumber, out var StarRA, out var StarDec, out SourceCode);
			GetEventDate(EventNumber, out var Year, out var _, out var _);
			if (!Kepler2)
			{
				return AsteroidNumber.PadLeft(7) + " " + AsteroidName.PadRight(16) + GetFormattedEventDate(EventNumber) + "   " + starID + "\r\n";
			}
			if ((Occult.Kepler2.NumKep2Stars < 1) & Occult.Kepler2.Kepler2DataExists)
			{
				Occult.Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			if (Occult.Kepler2.StarInKepler2(StarRA * (180.0 / Math.PI), StarDec * (180.0 / Math.PI), Year - 2000, out var RecNum))
			{
				return AsteroidNumber.PadLeft(7) + " " + AsteroidName.PadRight(16) + GetFormattedEventDate(EventNumber) + starID.PadRight(26) + Occult.Kepler2.K2[RecNum].EPIC_ID + "  " + Utilities.DEGtoDMS(StarRA * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false) + " " + Utilities.DEGtoDMS(StarDec * (180.0 / Math.PI), 3, 1, MinutesOnly: false) + "\r\n";
			}
			return "";
		}

		internal void GetDiameters()
		{
			Asteroid_Observations_Reports.DiameterList.Clear();
			for (int i = 0; i < OccEvents.Count; i++)
			{
				AsteroidDiameters asteroidDiameters = new AsteroidDiameters();
				asteroidDiameters.ShapeModelDiameter = new List<int>();
				asteroidDiameters.ShapeModelNum = new List<string>();
				asteroidDiameters.ShapeModelSource = new List<string>();
				asteroidDiameters.ShapeModelFit = new List<string>();
				bool flag = false;
				if (OccEvents[i].IsXML)
				{
					for (int j = 0; j < OccEvents[i].Lines.Count; j++)
					{
						int result;
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[1]))
						{
							string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
								.Split(new char[1] { '|' });
							int.TryParse(array[0], out result);
							asteroidDiameters.Year = result;
							int.TryParse(array[1], out result);
							asteroidDiameters.Month = result;
							int.TryParse(array[2], out result);
							asteroidDiameters.Day = result;
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
						{
							string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
								.Split(new char[1] { '|' });
							asteroidDiameters.AsteroidNumber = array2[0].Trim();
							asteroidDiameters.AsteroidID = Utilities.HTML_DecodeString(array2[1].Replace("barycenter", ""));
							if (asteroidDiameters.AsteroidNumber == "134340")
							{
								asteroidDiameters.AsteroidNumber = "P9M00";
							}
							int result2 = 0;
							if (!int.TryParse(asteroidDiameters.AsteroidNumber, out result2))
							{
								result2 = 0;
							}
							asteroidDiameters.HasShapeModel = false;
							if (result2 > 0)
							{
								asteroidDiameters.HasShapeModel = DisplayMPOccultations.IsInISAM(result2);
								if (!asteroidDiameters.HasShapeModel)
								{
									asteroidDiameters.HasShapeModel = DisplayMPOccultations.IsInDAMIT(result2);
								}
							}
							Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(result2, out var Diameter, out var DiameterUncertainty, out var _);
							asteroidDiameters.IRall_Dia = Diameter;
							asteroidDiameters.IRall_Sdev = DiameterUncertainty;
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[5]))
						{
							string[] array3 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[5], "").Replace(Tags.TagEnd[5], "")
								.Split(new char[1] { '|' });
							asteroidDiameters.Solve_Circular = array3[5] == "1";
						}
						double result3;
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[6]))
						{
							string[] array4 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
								.Split(new char[1] { '|' });
							double.TryParse(array4[2], out result3);
							asteroidDiameters.X_Dia = result3;
							double.TryParse(array4[3], out result3);
							asteroidDiameters.Y_Dia = result3;
							double.TryParse(array4[4], out result3);
							asteroidDiameters.PA = result3;
							asteroidDiameters.EllipseDiameter = Math.Sqrt(asteroidDiameters.X_Dia * asteroidDiameters.Y_Dia);
							int.TryParse(array4[5], out result);
							asteroidDiameters.Quality = result;
							asteroidDiameters.SolveUsingAssumedDiameter = array4[6] == "1";
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[7]))
						{
							string[] array5 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[7], "").Replace(Tags.TagEnd[7], "")
								.Split(new char[1] { '|' });
							double.TryParse(array5[2], out result3);
							asteroidDiameters.Sdev_Major = result3;
							double.TryParse(array5[3], out result3);
							asteroidDiameters.Sdev_Minor = result3;
							double.TryParse(array5[4], out result3);
							asteroidDiameters.Sdev_PA = result3;
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[9]))
						{
							string[] array6 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[9], "").Replace(Tags.TagEnd[9], "")
								.Split(new char[1] { '|' });
							asteroidDiameters.ShapeModelFit.Add(array6[3]);
							if ("3456".Contains(array6[3]))
							{
								flag = true;
							}
							asteroidDiameters.ShapeModelSource.Add(array6[0].PadRight(1).Substring(0, 1));
							if (array6[0].PadRight(1) == "ISAM")
							{
								asteroidDiameters.ShapeModelNum.Add(array6[1].PadRight(5).Substring(1));
							}
							else
							{
								asteroidDiameters.ShapeModelNum.Add(array6[1].PadRight(4));
							}
							double.TryParse(array6[5], out var result4);
							double.TryParse(array6[6], out var result5);
							if (result5 == 0.0)
							{
								result5 = result4;
							}
							if (result4 == 0.0)
							{
								result4 = result5;
							}
							asteroidDiameters.ShapeModelDiameter.Add(Convert.ToInt32((result4 + result5) / 2.0));
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[13]))
						{
							string[] array7 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[13], "").Replace(Tags.TagEnd[13], "")
								.Split(new char[1] { '|' });
							asteroidDiameters.NumChords = int.Parse(array7[8]);
						}
					}
				}
				if (((asteroidDiameters.Quality > 1) | (asteroidDiameters.ShapeModelDiameter.Count > 0)) || flag)
				{
					Asteroid_Observations_Reports.DiameterList.Add(asteroidDiameters);
				}
			}
		}

		internal void GetEllipseDiameter()
		{
			Asteroid_Observations_Reports.EllipseDiameterList.Clear();
			for (int i = 0; i < OccEvents.Count; i++)
			{
				EllipseDiameters ellipseDiameters = new EllipseDiameters();
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
					{
						string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						if (array[0].Contains("P") | (array[0].Length == 0))
						{
							continue;
						}
						ellipseDiameters.AsteroidNumber = int.Parse(array[0].Trim());
						ellipseDiameters.AsteroidName = array[1].Replace("barycenter", "");
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[5]))
					{
						string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[5], "").Replace(Tags.TagEnd[5], "")
							.Split(new char[1] { '|' });
						ellipseDiameters.Solve_Circular = array2[5] == "1";
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[6]))
					{
						string[] array3 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
							.Split(new char[1] { '|' });
						double.TryParse(array3[2], out var result);
						double.TryParse(array3[3], out var result2);
						ellipseDiameters.EllipseDiameter = Math.Sqrt(result * result2);
						int.TryParse(array3[5], out var result3);
						ellipseDiameters.Quality = result3;
						ellipseDiameters.SolveAssumed = array3[6] == "1";
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[13]))
					{
						string[] array4 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[13], "").Replace(Tags.TagEnd[13], "")
							.Split(new char[1] { '|' });
						ellipseDiameters.NumChords = int.Parse(array4[8]);
					}
				}
				if (ellipseDiameters.AsteroidNumber != 0 && ((ellipseDiameters.Quality > 2) & (ellipseDiameters.NumChords > 1)))
				{
					Asteroid_Observations_Reports.EllipseDiameterList.Add(ellipseDiameters);
				}
			}
		}

		internal int GetEventQuality(int EventNumber)
		{
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[6]))
					{
						int.TryParse(OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
							.Split(new char[1] { '|' })[5], out var result);
						return result;
					}
				}
				return 0;
			}
			return int.Parse(OccEvents[EventNumber].Lines[1].Substring(63, 1));
		}

		internal double GetParallax(int EventNumber, out double dParallax)
		{
			dParallax = 0.0;
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[4]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						double.TryParse(array[8], out var result);
						if (array.Length == 12)
						{
							double.TryParse(array[9], out dParallax);
						}
						return result;
					}
				}
				return 0.0;
			}
			return double.Parse(OccEvents[EventNumber].Lines[0].Substring(101, 7));
		}

		internal void GetDoubles(bool All)
		{
			Asteroid_Observations_Reports.DoublesList.Clear();
			int num = OccEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GetEventDoubleData(i);
			}
			DisplayDoubleStars.MatchToWDS();
			AsteroidDoubleStars.SortField = 2;
			Asteroid_Observations_Reports.DoublesList.Sort();
		}

		private void GetEventDoubleData(int EventNumber)
		{
			string companionID = "";
			int Year = 1900;
			int Month = 1;
			int Day = 1;
			double Hour = 0.0;
			double num = -1.0;
			double mv = 25.0;
			double mv2 = 25.0;
			double StarDia_mas = 0.0;
			bool flag = false;
			bool flag2 = false;
			for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
			{
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[6]) && OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[6]))
				{
					string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
						.Split(new char[1] { '|' });
					int.TryParse(array[5], out var result);
					if (result == 6)
					{
						flag = true;
						flag2 = true;
					}
				}
				if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[16]))
				{
					flag2 = true;
					break;
				}
			}
			if (!flag2)
			{
				return;
			}
			GetAsteroidID(EventNumber, out var AsteroidNumber, out var AsteroidName);
			GetEventDate(EventNumber, out Year, out Month, out Day, out Hour);
			string text = GetStarID(EventNumber).PadRight(18);
			GetStarCoords(EventNumber, out var StarRA, out var StarDec, out var _, out var StarMag, out var _, out var _, out StarDia_mas);
			num = GetStarRUWE(EventNumber);
			string jDSO_Vol_Num_Pg;
			string jDSO_SubmitDate = (jDSO_Vol_Num_Pg = "");
			for (int j = 0; j < OccEvents[EventNumber].Lines.Count; j++)
			{
				if (OccEvents[EventNumber].Lines[j].Contains(Tags.TagStart[34]))
				{
					string[] array = OccEvents[EventNumber].Lines[j].Trim().Replace(Tags.TagStart[34], "").Replace(Tags.TagEnd[34], "")
						.Split(new char[1] { '|' });
					double.TryParse(array[4], out var result2);
					double num2 = Math.Pow(10.0, StarMag / -2.5);
					double d = num2 * result2 / (1.0 + result2);
					double d2 = num2 / (1.0 + result2);
					mv = -2.5 * Math.Log10(d);
					mv2 = -2.5 * Math.Log10(d2);
					companionID = array[10];
					break;
				}
			}
			for (int k = 0; k < OccEvents[EventNumber].Lines.Count; k++)
			{
				if (OccEvents[EventNumber].Lines[k].Contains(Tags.TagStart[35]))
				{
					jDSO_SubmitDate = (jDSO_Vol_Num_Pg = "");
					string[] array = OccEvents[EventNumber].Lines[k].Trim().Replace(Tags.TagStart[35], "").Replace(Tags.TagEnd[35], "")
						.Split(new char[1] { '|' });
					if (array.Length > 1)
					{
						jDSO_SubmitDate = array[0];
						jDSO_Vol_Num_Pg = array[1];
					}
					break;
				}
			}
			List<DoubleStarObservers> DSOlist = new List<DoubleStarObservers>();
			for (int l = 0; l < OccEvents[EventNumber].Lines.Count; l++)
			{
				if (OccEvents[EventNumber].Lines[l].Contains(Tags.TagStart[21]))
				{
					string[] array = OccEvents[EventNumber].Lines[l].ToString().Trim().Replace(Tags.TagStart[21], "")
						.Replace(Tags.TagEnd[21], "")
						.Replace("&apos;", "'")
						.Split(new char[1] { '|' });
					Format_Add_Name(array[1].Trim().Replace(". ", " ").Replace(".", " ")
						.Trim(), ref DSOlist);
					Format_Add_Name(array[2].Trim().Replace(". ", " ").Replace(".", " ")
						.Trim(), ref DSOlist);
				}
			}
			DSOlist.Sort();
			for (int num3 = DSOlist.Count - 2; num3 >= 0; num3--)
			{
				if (DSOlist[num3].Observer == DSOlist[num3 + 1].Observer)
				{
					DSOlist.RemoveAt(num3 + 1);
				}
			}
			string text2 = "";
			for (int m = 0; m < DSOlist.Count - 1; m++)
			{
				text2 = text2 + DSOlist[m].Observer + ", ";
			}
			text2 += DSOlist[DSOlist.Count - 1].Observer;
			int num4 = 0;
			for (int n = 0; n < OccEvents[EventNumber].Lines.Count; n++)
			{
				if (!OccEvents[EventNumber].Lines[n].Contains(Tags.TagStart[17]))
				{
					continue;
				}
				AsteroidDoubleStars asteroidDoubleStars = new AsteroidDoubleStars();
				string[] array = OccEvents[EventNumber].Lines[n].Trim().Replace(Tags.TagStart[17], "").Replace(Tags.TagEnd[17], "")
					.Split(new char[1] { '|' });
				double.TryParse(array[1], out var result3);
				if (result3 != 0.0)
				{
					asteroidDoubleStars.AsteroidID = AsteroidName;
					asteroidDoubleStars.AsteroidNumber = AsteroidNumber;
					asteroidDoubleStars.Year = Year;
					asteroidDoubleStars.Month = Month;
					asteroidDoubleStars.Day = Day;
					asteroidDoubleStars.Hour = Hour;
					asteroidDoubleStars.NumberNights = 1;
					asteroidDoubleStars.StarID = text.Trim();
					asteroidDoubleStars.Mv1 = mv;
					asteroidDoubleStars.Mv2 = mv2;
					asteroidDoubleStars.CompanionID = companionID;
					asteroidDoubleStars.RUWE = num;
					asteroidDoubleStars.StarDia_mas = StarDia_mas;
					asteroidDoubleStars.RA2000 = StarRA;
					asteroidDoubleStars.Dec2000 = StarDec;
					asteroidDoubleStars.JDSO_SubmitDate = jDSO_SubmitDate;
					asteroidDoubleStars.JDSO_Vol_Num_Pg = jDSO_Vol_Num_Pg;
					asteroidDoubleStars.PA_deg = double.Parse(array[0]);
					if (asteroidDoubleStars.PA_deg < 0.0)
					{
						asteroidDoubleStars.PA_deg += 360.0;
					}
					asteroidDoubleStars.SingleComponentOnly = false;
					asteroidDoubleStars.Sep_mas = double.Parse(array[1]);
					if (asteroidDoubleStars.Sep_mas < 0.0)
					{
						asteroidDoubleStars.SingleComponentOnly = true;
					}
					double.TryParse(array[2], out result3);
					asteroidDoubleStars.Sdev_PA_Star_deg = result3;
					if (asteroidDoubleStars.SingleComponentOnly)
					{
						asteroidDoubleStars.Sdev_PA_Star_deg = 45.0;
					}
					double.TryParse(array[3], out result3);
					asteroidDoubleStars.Sdev_Sep_Star_mas = result3;
					int.TryParse(array[8], out var _);
					num4 = (asteroidDoubleStars.SolutionNumber = num4 + 1);
					asteroidDoubleStars.Observers = text2;
					Asteroid_Observations_Reports.DoublesList.Add(asteroidDoubleStars);
				}
			}
			if (flag && num4 == 0)
			{
				AsteroidDoubleStars asteroidDoubleStars2 = new AsteroidDoubleStars();
				asteroidDoubleStars2.AsteroidID = AsteroidName;
				asteroidDoubleStars2.AsteroidNumber = AsteroidNumber;
				asteroidDoubleStars2.Year = Year;
				asteroidDoubleStars2.Month = Month;
				asteroidDoubleStars2.Day = Day;
				asteroidDoubleStars2.StarID = text.Trim();
				asteroidDoubleStars2.SingleComponentOnly = true;
				asteroidDoubleStars2.Mv1 = StarMag + 0.75;
				asteroidDoubleStars2.Mv2 = StarMag + 0.75;
				asteroidDoubleStars2.RUWE = num;
				asteroidDoubleStars2.RA2000 = StarRA;
				asteroidDoubleStars2.Dec2000 = StarDec;
				asteroidDoubleStars2.JDSO_SubmitDate = jDSO_SubmitDate;
				asteroidDoubleStars2.JDSO_Vol_Num_Pg = jDSO_Vol_Num_Pg;
				asteroidDoubleStars2.PA_deg = -1.0;
				asteroidDoubleStars2.Sep_mas = -1000.0;
				asteroidDoubleStars2.Sdev_PA_Star_deg = 0.0;
				asteroidDoubleStars2.Sdev_Sep_Star_mas = 0.0;
				asteroidDoubleStars2.SolutionNumber = 1;
				asteroidDoubleStars2.Observers = text2;
				Asteroid_Observations_Reports.DoublesList.Add(asteroidDoubleStars2);
			}
		}

		private static void Format_Add_Name(string Name, ref List<DoubleStarObservers> DSOlist)
		{
			string text = Name.ToLower();
			if ((Name.Length > 2) & !text.Contains("prediction") & !text.Contains("unknown") & !text.Contains("anonymous") & !Name.Contains("?") & !Name.Contains("Uni"))
			{
				DoubleStarObservers doubleStarObservers = new DoubleStarObservers();
				doubleStarObservers.Observer = Utilities.InitialPlusName(Utilities.ProperCase(Name.Trim().Replace(". ", " ").Replace(".", " ")).Trim());
				DSOlist.Add(doubleStarObservers);
			}
		}

		internal void GetRUWEcounts(int StartYear, int EndYear, out int[] RUWE)
		{
			Statistics.RUWE_ids.Clear();
			RUWE = new int[201];
			for (int i = 0; i < OccEvents.Count; i++)
			{
				GetEventDateOfAstrometry(i, out var Year, out var _, out var _, out var _);
				if (Year >= StartYear && Year <= EndYear)
				{
					double starRUWE = GetStarRUWE(i);
					int num = (int)(GetStarRUWE(i) * 20.0);
					if ((num >= 0) & (num < RUWE.Length - 1))
					{
						RUWE[num]++;
					}
					else if ((num >= 0) & (num >= RUWE.Length - 1))
					{
						RUWE[RUWE.Length - 1]++;
					}
					if (starRUWE >= 1.4)
					{
						RUWE_IDs rUWE_IDs = new RUWE_IDs();
						rUWE_IDs.RUWE = starRUWE;
						rUWE_IDs.ID = GetEventID(i, IncludeMagnitude: false, Kepler2: false);
						Statistics.RUWE_ids.Add(rUWE_IDs);
					}
				}
			}
			Statistics.RUWE_ids.Sort();
		}

		internal void GetEventBinaryData()
		{
			int Year = 1900;
			int Month = 1;
			int Day = 1;
			double Hour = 0.0;
			Asteroid_Observations_Reports.BinaryList.Clear();
			int num = OccEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (!OccEvents[i].Lines[j].Contains(Tags.TagStart[10]))
					{
						continue;
					}
					GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
					string text = GetStarID(i).PadRight(18);
					string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
						.Split(new char[1] { '|' });
					if (!(double.Parse(array[5]) <= 0.0))
					{
						GetEventDateOfAstrometry(i, out Year, out Month, out Day, out Hour);
						BinaryAsteroid binaryAsteroid = new BinaryAsteroid();
						binaryAsteroid.AsteroidID = AsteroidName;
						binaryAsteroid.AsteroidNumber = AsteroidNumber;
						binaryAsteroid.Year = Year;
						binaryAsteroid.Month = Month;
						binaryAsteroid.Day = Day;
						binaryAsteroid.Hour = Hour;
						binaryAsteroid.SatelliteID = array[0];
						binaryAsteroid.StarID = text.Trim();
						binaryAsteroid.Sep = double.Parse(array[5]);
						binaryAsteroid.PA = double.Parse(array[6]);
						if (binaryAsteroid.PA < 0.0)
						{
							binaryAsteroid.PA += 360.0;
						}
						double.TryParse(array[9], out var result);
						binaryAsteroid.MajorAxisCompanion = result;
						double.TryParse(array[10], out result);
						binaryAsteroid.MinorAxisCompanion = result;
						double.TryParse(array[11], out result);
						binaryAsteroid.PAMajorAxisCompanion = result;
						int.TryParse(array[12], out var result2);
						binaryAsteroid.FitQualityCompanion = result2;
						if (array.Length > 13)
						{
							binaryAsteroid.CBET = array[13];
						}
						else
						{
							binaryAsteroid.CBET = "";
						}
						Asteroid_Observations_Reports.BinaryList.Add(binaryAsteroid);
						break;
					}
				}
			}
		}

		internal string GetAsteroidsWithRings()
		{
			int Year = 1900;
			int Month = 1;
			int Day = 1;
			string text = "Asteroids with Ring event observations\r\n\r\n";
			int num = OccEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				bool flag = false;
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if ((OccEvents[i].Lines[j].Contains(Tags.TagStart[23]) | OccEvents[i].Lines[j].Contains(Tags.TagStart[24])) && OccEvents[i].Lines[j].Split(new char[1] { '|' })[1] == "N")
					{
						flag = true;
						break;
					}
				}
				if (flag)
				{
					GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
					GetEventDate(i, out Year, out Month, out Day);
					text = text + Year + " " + Utilities.ShortMonths[Month] + " " + Day.ToString().PadLeft(2, '0') + AsteroidNumber.PadLeft(9) + " " + AsteroidName + "\r\n";
				}
			}
			return text;
		}

		internal void GetEventsWithShapeModelsAvailable()
		{
			string text = "";
			int result = 0;
			Asteroid_Observations_Reports.AsteroidsHavingShapes.Clear();
			int num = OccEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (OccEvents[i].IndexLine.Contains("#") | OccEvents[i].IndexLine.Contains("†"))
				{
					AsteroidsWithShapes asteroidsWithShapes = new AsteroidsWithShapes();
					text = OccEvents[i].IndexLine;
					asteroidsWithShapes.AsteroidID = text.Substring(19, 14).Trim();
					int.TryParse(text.Substring(11, 7), out result);
					asteroidsWithShapes.AsteroidNumber = result;
					asteroidsWithShapes.Count = 1;
					Asteroid_Observations_Reports.AsteroidsHavingShapes.Add(asteroidsWithShapes);
				}
			}
			AsteroidsWithShapes.SortField = 0;
			Asteroid_Observations_Reports.AsteroidsHavingShapes.Sort();
			for (int num2 = Asteroid_Observations_Reports.AsteroidsHavingShapes.Count - 1; num2 > 0; num2--)
			{
				if (Asteroid_Observations_Reports.AsteroidsHavingShapes[num2].AsteroidID == Asteroid_Observations_Reports.AsteroidsHavingShapes[num2 - 1].AsteroidID)
				{
					Asteroid_Observations_Reports.AsteroidsHavingShapes[num2 - 1].Count += Asteroid_Observations_Reports.AsteroidsHavingShapes[num2].Count;
					Asteroid_Observations_Reports.AsteroidsHavingShapes.RemoveAt(num2);
				}
			}
		}

		internal void GetShapeModels(string AsteroidID)
		{
			bool flag = false;
			string text = "";
			Asteroid_Observations_Reports.ShapesList.Clear();
			int num = OccEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (!(OccEvents[i].IndexLine.Substring(18, 14).Trim() == AsteroidID))
				{
					continue;
				}
				GetEventDate(i, out var Year, out var Month, out var Day);
				GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
				text = GetStarID(i);
				flag = false;
				int chords = int.Parse(OccEvents[i].IndexLine.Substring(37, 3).Trim());
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[9]))
					{
						flag = true;
						ShapeModels shapeModels = new ShapeModels();
						shapeModels.Year = Year;
						shapeModels.Month = Month;
						shapeModels.Day = Day;
						shapeModels.StarID = text;
						shapeModels.AsteroidID = AsteroidName;
						shapeModels.AsteroidNumber = AsteroidNumber;
						string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[9], "").Replace(Tags.TagEnd[9], "")
							.Split(new char[1] { '|' });
						shapeModels.Source = array[0];
						shapeModels.ID = array[1];
						double.TryParse(array[2], out var result);
						shapeModels.Surface_VolRatio = result;
						shapeModels.FitQuality = int.Parse(array[3]);
						int.TryParse(array[4], out var result2);
						shapeModels.PhaseCorrection = result2;
						double.TryParse(array[5], out result);
						shapeModels.VolumeDia_Min = result;
						double.TryParse(array[6], out result);
						shapeModels.VolumeDia_Max = result;
						shapeModels.VersionInfo = array[7];
						shapeModels.Chords = chords;
						Asteroid_Observations_Reports.ShapesList.Add(shapeModels);
					}
				}
				if (!flag)
				{
					ShapeModels shapeModels = new ShapeModels();
					shapeModels.Year = Year;
					shapeModels.Month = Month;
					shapeModels.Day = Day;
					shapeModels.StarID = text;
					shapeModels.AsteroidID = AsteroidName;
					shapeModels.AsteroidNumber = AsteroidNumber;
					shapeModels.VersionInfo = "No shape model fitted";
					shapeModels.Chords = chords;
					Asteroid_Observations_Reports.ShapesList.Add(shapeModels);
				}
			}
		}

		internal void GetEventsWhereShapeModelIsOutOfDate()
		{
			Asteroid_Observations_Reports.DAMITmodelsRemoved.Clear();
			Asteroid_Observations_Reports.DAMITmodelsUpdated.Clear();
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			bool flag = false;
			int num4 = OccEvents.Count - 1;
			GetShapeModelData.InitialiseDAMITShapeModels();
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 1;
			GetShapeModelData.DAMITModelsByAsteroidNumber.Sort();
			for (int i = 0; i <= num4; i++)
			{
				if (!(OccEvents[i].IndexLine.Contains("#") | OccEvents[i].IndexLine.Contains("†")))
				{
					continue;
				}
				GetEventDate(i, out var Year, out var Month, out var Day);
				GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
				DateTime date_LastEdited = GetDate_LastEdited(i);
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (!OccEvents[i].Lines[j].Contains(Tags.TagStart[9]))
					{
						continue;
					}
					string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[9], "").Replace(Tags.TagEnd[9], "")
						.Split(new char[1] { '|' });
					if (!int.TryParse(array[1], out var result) || !(array[0] == "DAMIT"))
					{
						continue;
					}
					num = 0;
					num2 = GetShapeModelData.DAMITModelsByAsteroidNumber.Count - 1;
					flag = false;
					do
					{
						num3 = (num2 + num) / 2;
						if (GetShapeModelData.DAMITModelsByAsteroidNumber[num3].ModelNumber_Numeric == result)
						{
							flag = true;
							break;
						}
						if (GetShapeModelData.DAMITModelsByAsteroidNumber[num3].ModelNumber_Numeric < result)
						{
							num = num3 + 1;
						}
						else
						{
							num2 = num3 - 1;
						}
					}
					while (num <= num2);
					if (flag)
					{
						string text = GetShapeModelData.DAMITModelsByAsteroidNumber[num3].Modified.Trim();
						DateTime dateTime;
						if ((text.Length > 0) & Enumerable.Contains(text, '-'))
						{
							string[] array2 = text.Split(new char[1] { '-' });
							dateTime = new DateTime(int.Parse(array2[0]), int.Parse(array2[1]), int.Parse(array2[2]));
						}
						else
						{
							dateTime = new DateTime(1990, 1, 1);
						}
						if (dateTime > date_LastEdited)
						{
							Asteroid_Observations_Reports.DAMITmodelsUpdated.Add(AsteroidNumber + "  " + AsteroidName.PadRight(12) + " on " + Year + " " + Utilities.ShortMonths[Month] + " " + Day.ToString().PadLeft(2, '0') + ": DAMIT model " + result.ToString().PadLeft(5));
						}
					}
					else
					{
						Asteroid_Observations_Reports.DAMITmodelsRemoved.Add(AsteroidNumber + "  " + AsteroidName.PadRight(12) + " on " + Year + " " + Utilities.ShortMonths[Month] + " " + Day.ToString().PadLeft(2, '0') + ": DAMIT model " + result.ToString().PadLeft(5));
					}
				}
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			GetShapeModelData.DAMITModelsByAsteroidNumber.Sort();
		}

		internal void GetDistantAsteroids()
		{
			Asteroid_Observations_Reports.DistantList.Clear();
			for (int i = 0; i < OccEvents.Count; i++)
			{
				DistantAsteroid distantAsteroid = new DistantAsteroid();
				if (OccEvents[i].IsXML)
				{
					int num = 0;
					for (int j = 0; j < OccEvents[i].Lines.Count; j++)
					{
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[1]))
						{
							string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
								.Split(new char[1] { '|' });
							distantAsteroid.Date = array[0] + array[1].PadLeft(3) + array[2].PadLeft(3);
							distantAsteroid.SortDate = Utilities.JD_from_Date(int.Parse(array[0]), int.Parse(array[1]), int.Parse(array[2]));
							num++;
						}
						if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
						{
							string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
								.Split(new char[1] { '|' });
							distantAsteroid.AsteroidNumber = array2[0];
							distantAsteroid.AsteroidID = array2[1];
							num++;
						}
						if (num > 1)
						{
							break;
						}
					}
					int.TryParse(distantAsteroid.AsteroidNumber, out var result);
					string text = AsteroidClassList.ClassOfAnAsteroid(result);
					if (text != "")
					{
						distantAsteroid.Class = text;
						Asteroid_Observations_Reports.DistantList.Add(distantAsteroid);
					}
				}
				else
				{
					string text2 = OccEvents[i].Lines[0];
					int.TryParse(text2.Substring(1, 6), out var result2);
					string text3 = AsteroidClassList.ClassOfAnAsteroid(result2);
					if (text3 != "")
					{
						distantAsteroid.AsteroidID = text2.Substring(8, 13);
						distantAsteroid.AsteroidNumber = text2.Substring(1, 6);
						distantAsteroid.Date = text2.Substring(21, 4) + " " + text2.Substring(25, 2) + " " + text2.Substring(27, 2);
						distantAsteroid.SortDate = Utilities.JD_from_Date(int.Parse(text2.Substring(21, 4)), int.Parse(text2.Substring(25, 2)), int.Parse(text2.Substring(27, 2)));
						distantAsteroid.Class = text3;
						Asteroid_Observations_Reports.DistantList.Add(distantAsteroid);
					}
				}
			}
		}

		internal void GetMPCdetails(int EventNumber, out string MPC_Date, out string MPC_Number, out string MPC_SubmissionID)
		{
			MPC_Number = (MPC_Date = (MPC_SubmissionID = ""));
			if (OccEvents[EventNumber].IsXML)
			{
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[15]))
					{
						string[] array = OccEvents[EventNumber].Lines[i].Trim().Replace(Tags.TagStart[15], "").Replace(Tags.TagEnd[15], "")
							.Split(new char[1] { '|' });
						MPC_Date = array[0];
						MPC_Number = array[1];
						if (array.Length > 2)
						{
							MPC_SubmissionID = array[2];
						}
						else
						{
							MPC_SubmissionID = "";
						}
					}
				}
			}
			else
			{
				MPC_Number = OccEvents[EventNumber].Lines[1].Substring(142, 7);
				MPC_Date = OccEvents[EventNumber].Lines[1].Substring(134, 8);
				MPC_SubmissionID = "";
			}
		}

		internal void SetMPCdetails(int EventNumber, string MPC_Date, string MPC_Number, string MPC_SubmissionID)
		{
			if (OccEvents[EventNumber].IsXML)
			{
				string value = "".PadLeft(Tags.TagIndent[15]) + Tags.TagStart[15] + MPC_Date.Trim() + "|" + MPC_Number.Trim() + "|" + MPC_SubmissionID + Tags.TagEnd[15];
				for (int i = 0; i < OccEvents[EventNumber].Lines.Count; i++)
				{
					if (OccEvents[EventNumber].Lines[i].Contains(Tags.TagStart[15]))
					{
						OccEvents[EventNumber].Lines[i] = value;
						break;
					}
				}
			}
			else
			{
				OccEvents[EventNumber].Lines[1] = OccEvents[EventNumber].Lines[1].Remove(134, 8).Insert(134, MPC_Date.PadLeft(8)).Remove(142, 7)
					.Insert(142, MPC_Number.PadLeft(7));
			}
		}

		internal string GetMidTErrors()
		{
			double result = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			string text = "";
			if (!OccEvents[0].IsXML)
			{
				text = "This requires the XML version to be loaded";
			}
			for (int i = 0; i < OccEvents.Count; i++)
			{
				int num4 = 0;
				num2 = 0.0;
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					string[] array;
					if (OccEvents[i].Lines[j].ToString().Contains(Tags.TagStart[1].ToString()))
					{
						array = OccEvents[i].Lines[j].ToString().Trim().Replace(Tags.TagStart[1], "")
							.Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						double.TryParse(array[3], out result);
						num = result;
					}
					if (OccEvents[i].Lines[j].ToString().Contains(Tags.TagStart[23].ToString()))
					{
						array = OccEvents[i].Lines[j].ToString().Trim().Replace(Tags.TagStart[23], "")
							.Replace(Tags.TagEnd[23], "")
							.Split(new char[1] { '|' });
						if (array[1] == "D")
						{
							num3 = Utilities.DegFromDMS_TextBoxes(array[0].Substring(0, 2), array[0].Substring(3, 2), array[0].Substring(6));
							if (num3 > 0.0)
							{
								num2 += num3;
								num4++;
							}
						}
					}
					if (!OccEvents[i].Lines[j].ToString().Contains(Tags.TagStart[24].ToString()))
					{
						continue;
					}
					array = OccEvents[i].Lines[j].ToString().Trim().Replace(Tags.TagStart[24], "")
						.Replace(Tags.TagEnd[24], "")
						.Split(new char[1] { '|' });
					if (array[1] == "R")
					{
						num3 = Utilities.DegFromDMS_TextBoxes(array[0].Substring(0, 2), array[0].Substring(3, 2), array[0].Substring(6));
						if (num3 > 0.0)
						{
							num2 += num3;
							num4++;
						}
					}
				}
				num2 /= (double)num4;
				if (Math.Abs(num - num2) > 0.25)
				{
					text = text + string.Format("  MidT={0,4:f1}  Ave={1,4:f1}  Diff={2:+0.0;-0.0}   ", num, num2, num2 - num) + GetEventID(i, IncludeMagnitude: false, Kepler2: false);
				}
			}
			return text;
		}

		internal void GetPositionsAndObservers(bool WithNames, bool Gaia_DR2_Updates, bool Asteroids, bool Planets, bool AsteroidSatellites, bool IncludeAlreadyReported, int YearStart, int YearEnd, double LastJD, int AsteroidNumber_only, string Observer, bool ObserverOnly, bool ExcludeStarsWithNoPM, bool IncludeExtraData)
		{
			bool flag = false;
			string text = "";
			string text2 = "";
			double result = 0.0;
			double result2 = 0.0;
			bool flag2 = false;
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Extracting Positions and Observers, from " + YearStart + " to " + YearEnd);
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(OccEvents.Count);
			((Control)pBar).Show();
			for (int i = 0; i < OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Application.DoEvents();
				text2 = (text = "");
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].ToString().Contains(Tags.TagStart[15].ToString()))
					{
						string[] array = OccEvents[i].Lines[j].ToString().Trim().Replace(Tags.TagStart[15], "")
							.Replace(Tags.TagEnd[15], "")
							.Split(new char[1] { '|' });
						text2 = array[0].Trim();
						text = array[1].Trim();
						array[2].Trim();
						break;
					}
				}
				if (!IncludeAlreadyReported && text2.Length > 0)
				{
					continue;
				}
				AsteroidPositions asteroidPositions = new AsteroidPositions();
				AsteroidPositions.IncludeExtraDataInXYZ = IncludeExtraData;
				SatellitePositions satellitePositions = new SatellitePositions();
				satellitePositions.SatelliteFitQuality = -100;
				asteroidPositions.UnseenPrimary = false;
				asteroidPositions.DoubleStar_NumSolns = 0;
				asteroidPositions.Satellites.Clear();
				double num = 0.0;
				int num2 = 0;
				double GeocentricX_atConj;
				double GeocentricY_atConj;
				for (int k = 0; k < OccEvents[i].Lines.Count; k++)
				{
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[1].ToString()))
					{
						double.TryParse(OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[2], "")
							.Replace(Tags.TagEnd[2], "")
							.Split(new char[1] { '|' })[3], out var result3);
						asteroidPositions.MidTforMotions = result3;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[3].ToString()))
					{
						int num3 = 0;
						int num4 = 0;
						if (VersionOfReadFile >= 2.0)
						{
							num3 = 1;
							num4 = 6;
						}
						string[] array2 = OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[3], "")
							.Replace(Tags.TagEnd[3], "")
							.Split(new char[1] { '|' });
						asteroidPositions.StarCat = array2[0];
						asteroidPositions.StarID = array2[0] + array2[1];
						asteroidPositions.GaiaVersion = array2[2];
						asteroidPositions.GaiaNumber = array2[3].PadLeft(18);
						asteroidPositions.StarJ2000RA_Deg = 15.0 * double.Parse(array2[3 + num3]);
						asteroidPositions.StarJ2000Dec_Deg = double.Parse(array2[4 + num3]);
						asteroidPositions.StarMag = double.Parse(array2[7 + num4]);
						if (VersionOfReadFile >= 2.0)
						{
							asteroidPositions.StarRA_Uncert_mas = double.Parse(array2[6]);
							asteroidPositions.StarDec_Uncert_mas = double.Parse(array2[7]);
							asteroidPositions.GaiaIssues = int.Parse(array2[9]);
							asteroidPositions.StarApparentRA_Deg = 15.0 * double.Parse(array2[10]);
							asteroidPositions.StarApparentDec_Deg = double.Parse(array2[11]);
						}
						else
						{
							int num6 = (asteroidPositions.GaiaIssues = 0);
							GeocentricX_atConj = (asteroidPositions.StarRA_Uncert_mas = (asteroidPositions.StarDec_Uncert_mas = num6));
						}
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[34].ToString()))
					{
						string[] array3 = OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[34], "")
							.Replace(Tags.TagEnd[34], "")
							.Split(new char[1] { '|' });
						double.TryParse(array3[0], out var result4);
						asteroidPositions.RUWE = result4;
						int.TryParse(array3[1], out var result5);
						asteroidPositions.DuplicateSource = result5;
						int.TryParse(array3[2], out result5);
						asteroidPositions.NoGaiaPM = result5;
						int.TryParse(array3[3], out result5);
						asteroidPositions.PMUsingUCAC4 = result5;
						double.TryParse(array3[6], out result4);
						asteroidPositions.RA_Offset_DoubleStar_mas = result4;
						double.TryParse(array3[7], out result4);
						asteroidPositions.Dec_Offset_DoubleStar_mas = result4;
						double.TryParse(array3[8], out result4);
						asteroidPositions.RA_Offset_DoubleStar_sDev_mas = result4;
						double.TryParse(array3[9], out result4);
						asteroidPositions.Dec_Offset_DoubleStar_sDev_mas = result4;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[4].ToString()))
					{
						string[] array4 = OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[4], "")
							.Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						asteroidPositions.AsteroidNumber = array4[0];
						asteroidPositions.AsteroidID = Utilities.HTML_DecodeString(array4[1]);
						double.TryParse(array4[8], out var result6);
						int num9 = 0;
						if (array4.Length >= 12)
						{
							num9 = 1;
							double.TryParse(array4[9], out var result7);
							double num10 = asteroidPositions.RefHour - asteroidPositions.MidTforMotions;
							if (num10 > 12.0)
							{
								num10 -= 24.0;
							}
							if (num10 < -12.0)
							{
								num10 += 24.0;
							}
							asteroidPositions.AsteroidParallax = result6 + result7 * num10;
						}
						else
						{
							asteroidPositions.AsteroidParallax = result6;
						}
						double.TryParse(array4[9 + num9], out var result8);
						asteroidPositions.AsteroidIRDiameter = result8 / 6378.137 * asteroidPositions.AsteroidParallax;
						double.TryParse(array4[2], out var result9);
						double.TryParse(array4[3], out var result10);
						asteroidPositions.AlongTrackMotion_masSec = asteroidPositions.AsteroidParallax * Math.Sqrt(result9 * result9 + result10 * result10) / 3600.0 * 1000.0;
						asteroidPositions.PA_Motion = Math.Atan2(result9, result10) * (180.0 / Math.PI);
						if (asteroidPositions.PA_Motion < 0.0)
						{
							asteroidPositions.PA_Motion += 360.0;
						}
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[5].ToString()))
					{
						string[] array5 = OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[5], "")
							.Replace(Tags.TagEnd[5], "")
							.Split(new char[1] { '|' });
						asteroidPositions.SolveX = array5[0] == "1";
						asteroidPositions.SolveY = array5[1] == "1";
					}
					string PA_Ellipse;
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[6].ToString()))
					{
						Parse_EllipticFit(OccEvents[i].Lines[k].ToString(), out var _, out var _, out var _, out var _, out PA_Ellipse, out var Quality, out var _, out var _, out var CentreOfMass_Offset_X, out var CentreOfMass_Offset_Y);
						asteroidPositions.FitQuality = int.Parse(Quality);
						asteroidPositions.CenterOfMass_X = CentreOfMass_Offset_X;
						asteroidPositions.CenterOfMass_Y = CentreOfMass_Offset_Y;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[10].ToString()))
					{
						string[] array6 = OccEvents[i].Lines[k].ToString().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
							.Split(new char[1] { '|' });
						satellitePositions.IAUSatelliteID = array6[0].Trim();
						if (satellitePositions.IAUSatelliteID == "IAU designation")
						{
							satellitePositions.IAUSatelliteID = "";
						}
						satellitePositions.SatelliteFitQuality = int.Parse(array6[12]);
						Parse_Satellite(OccEvents[i].Lines[k].ToString(), out PA_Ellipse, out var _, out var _);
						satellitePositions.Sep_Conj_Star = double.Parse(array6[5]) / 1000.0;
						satellitePositions.PA_Conj_Star2000 = double.Parse(array6[6]);
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[12].ToString()))
					{
						Parse_ReferenceTime(OccEvents[i].Lines[k].ToString(), out var Year, out var Month, out var Day, out var Hour, out var RefHour_forAnalysis_Uncert_secs, out var RefHour_forAnalysis_AcrossPathUncertainty_mas);
						asteroidPositions.RefYear = Year;
						asteroidPositions.RefMonth = Month;
						asteroidPositions.RefDay = Day;
						asteroidPositions.RefHour = Hour;
						asteroidPositions.RefTimeUncert_Secs = RefHour_forAnalysis_Uncert_secs;
						asteroidPositions.RefHour_forAnalysis_AcrossPathUncertainty_mas = RefHour_forAnalysis_AcrossPathUncertainty_mas;
						num = Utilities.JD_from_Date(Year, Month, (double)Day + Hour / 24.0);
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[13].ToString()))
					{
						string[] array7 = OccEvents[i].Lines[k].ToString().Trim().Replace(Tags.TagStart[13], "")
							.Replace(Tags.TagEnd[13], "")
							.Split(new char[1] { '|' });
						Parse_MainBodyLine(OccEvents[i].Lines[k].ToString(), out var IAU_ID, out var X2, out var Y2, out var dRACosDec_atEvent, out var dDec_atEvent, out var Sdev_dRACosDec_atEvent, out var Sdev_dDec_atEvent, out var AstrometryShapeModelCentered, out var Number_Chords, out var UnseenBinaryPrimary, out var FitCode);
						asteroidPositions.IAUAsteroidID = IAU_ID;
						asteroidPositions.FPlane_X = X2;
						asteroidPositions.FPlane_Y = Y2;
						asteroidPositions.XYZ_DecimalPlaces = array7[1].Substring(array7[1].IndexOf(".") + 1).Length.ToString();
						asteroidPositions.dRA_fromStar = dRACosDec_atEvent;
						asteroidPositions.dDec_fromStar = dDec_atEvent;
						asteroidPositions.Sdev_dRA_fromStar = Sdev_dRACosDec_atEvent;
						asteroidPositions.Sdev_dDec_fromStar = Sdev_dDec_atEvent;
						asteroidPositions.Number_Chords = Number_Chords;
						asteroidPositions.ShapeModelCentered = AstrometryShapeModelCentered;
						asteroidPositions.UnseenPrimary = UnseenBinaryPrimary;
						asteroidPositions.FitCode = FitCode;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[14].ToString()))
					{
						Parse_SecondaryLine(OccEvents[i].Lines[k].ToString(), out var IAU_ID2, out var GeocentricX_atEvent, out var GeocentricY_atEvent, out var dRACosDec_Sat_atEvent, out var dDec_Sat_atEvent, out var _, out var _, out var Number_Chords_Sat);
						satellitePositions.IAUSatelliteID = IAU_ID2;
						satellitePositions.FPlane_X = GeocentricX_atEvent;
						satellitePositions.FPlane_Y = GeocentricY_atEvent;
						Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(num, asteroidPositions.StarJ2000RA_Deg / (180.0 / Math.PI), asteroidPositions.StarJ2000Dec_Deg / (180.0 / Math.PI), use2006values_Not1976: false, Utilities.JD_from_Date(asteroidPositions.RefYear, asteroidPositions.RefMonth, (double)asteroidPositions.RefDay + asteroidPositions.RefHour / 24.0), satellitePositions.FPlane_X, satellitePositions.FPlane_Y, out var EquatorialJ2000_X, out var EquatorialJ2000_Y, out var EquatorialJ2000_Z);
						satellitePositions.Equatorial_X = EquatorialJ2000_X;
						satellitePositions.Equatorial_Y = EquatorialJ2000_Y;
						satellitePositions.Equatorial_Z = EquatorialJ2000_Z;
						satellitePositions.dRA_fromStar_Satellite = dRACosDec_Sat_atEvent;
						satellitePositions.dDec_fromStar_Satellite = dDec_Sat_atEvent;
						satellitePositions.Sdev_dRA_fromStar_Satellite = asteroidPositions.Sdev_dRA_fromStar;
						satellitePositions.Sdev_dDec_fromStar_Satellite = asteroidPositions.Sdev_dDec_fromStar;
						satellitePositions.Number_ChordsSatellite = Number_Chords_Sat;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[17].ToString()))
					{
						asteroidPositions.DoubleStar_NumSolns++;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[29].ToString()))
					{
						Parse_AtConjunction(OccEvents[i].Lines[k].ToString(), out PA_Ellipse, out GeocentricX_atConj, out GeocentricY_atConj, out var Year_Conj, out var Month_Conj, out var Day_Conj, out var SDev_Day_Conj, out var sDev_AlongTrack, out var SeparationAtConj, out var sDevSeparationAtConj, out var PA_Conj, out var _);
						asteroidPositions.Year_Conj = Year_Conj;
						asteroidPositions.Month_Conj = Month_Conj;
						asteroidPositions.Day_Conj = Day_Conj;
						asteroidPositions.Sdev_T_Conj = SDev_Day_Conj;
						asteroidPositions.Sdev_AlongTrack_fromFit = sDev_AlongTrack;
						asteroidPositions.Sep_Conj = SeparationAtConj;
						asteroidPositions.Sdev_AcrossTrack_fromFit = sDevSeparationAtConj;
						asteroidPositions.PA_Conj_J2000 = PA_Conj;
					}
					if (OccEvents[i].Lines[k].ToString().Contains(Tags.TagStart[31].ToString()))
					{
						Parse_Secondary_AtConjunction(OccEvents[i].Lines[k].ToString(), out var Year_Conj2, out var Month_Conj2, out var Day_Conj2, out var Sdev_T_Conj, out var Sdev_AlongTrack, out var Sep_Conj_Star, out var sDev_Sep_Conj, out var PA_Conj_Star);
						satellitePositions.Year_Conj = Year_Conj2;
						satellitePositions.Month_Conj = Month_Conj2;
						satellitePositions.Day_Conj = Day_Conj2;
						satellitePositions.Sdev_T_Conj = Sdev_T_Conj;
						satellitePositions.Sdev_AlongTrack_fromFit = Sdev_AlongTrack;
						satellitePositions.Sep_Conj_Star = Sep_Conj_Star;
						satellitePositions.PA_Conj_Star2000 = PA_Conj_Star;
						satellitePositions.AcrossTrack_fromFit_Satellite = sDev_Sep_Conj;
						asteroidPositions.Satellites.Add(satellitePositions);
						num2++;
						satellitePositions = new SatellitePositions();
						satellitePositions.SatelliteFitQuality = -100;
					}
				}
				if (int.Parse(asteroidPositions.GaiaVersion) > 1 && num > 2410000.0)
				{
					Gaia.Gaia_FrameRotationCorrections(int.Parse(asteroidPositions.GaiaVersion), num, asteroidPositions.StarJ2000RA_Deg / (180.0 / Math.PI), asteroidPositions.StarJ2000Dec_Deg / (180.0 / Math.PI), asteroidPositions.StarMag, out var dRA_asec, out var dDec_asec, out var dRA_asec_Uncert, out var dDec_asec_Uncert);
					asteroidPositions.GaiaDR2_FrameRotation_RA_asec = dRA_asec;
					asteroidPositions.GaiaDR2_FrameRotationUncert_RA_asec = dRA_asec_Uncert;
					asteroidPositions.GaiaDR2_FrameRotation_Dec_asec = dDec_asec;
					asteroidPositions.GaiaDR2_FrameRotationUncert_Dec_asec = dDec_asec_Uncert;
				}
				else
				{
					double num12 = (asteroidPositions.GaiaDR2_FrameRotationUncert_Dec_asec = 0.0);
					GeocentricX_atConj = (asteroidPositions.GaiaDR2_FrameRotation_Dec_asec = num12);
					GeocentricY_atConj = (asteroidPositions.GaiaDR2_FrameRotation_RA_asec = (asteroidPositions.GaiaDR2_FrameRotationUncert_RA_asec = GeocentricX_atConj));
				}
				Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(num, asteroidPositions.StarJ2000RA_Deg / (180.0 / Math.PI), asteroidPositions.StarJ2000Dec_Deg / (180.0 / Math.PI), use2006values_Not1976: false, Utilities.JD_from_Date(asteroidPositions.RefYear, asteroidPositions.RefMonth, (double)asteroidPositions.RefDay + asteroidPositions.RefHour / 24.0), asteroidPositions.FPlane_X + asteroidPositions.CenterOfMass_X, asteroidPositions.FPlane_Y + asteroidPositions.CenterOfMass_Y, out var EquatorialJ2000_X2, out var EquatorialJ2000_Y2, out var EquatorialJ2000_Z2);
				asteroidPositions.Equatorial_X = EquatorialJ2000_X2;
				asteroidPositions.Equatorial_Y = EquatorialJ2000_Y2;
				asteroidPositions.Equatorial_Z = EquatorialJ2000_Z2;
				Utilities.Relativistic_Differential_Correction(Utilities.JD_from_Date(asteroidPositions.RefYear, asteroidPositions.RefMonth, (double)asteroidPositions.RefDay + asteroidPositions.RefHour / 24.0), asteroidPositions.StarJ2000RA_Deg / (180.0 / Math.PI), asteroidPositions.StarJ2000Dec_Deg / (180.0 / Math.PI), 8.794143836182533 / asteroidPositions.AsteroidParallax, out var dRA_ComparedToStar, out var dDec_ComparedToStar);
				asteroidPositions.Deflection_RA_fromStar = (0.0 - dRA_ComparedToStar) * Math.Cos(asteroidPositions.StarJ2000Dec_Deg / (180.0 / Math.PI)) * (180.0 / Math.PI) * 3600.0;
				asteroidPositions.Deflection_Dec_fromStar = (0.0 - dDec_ComparedToStar) * (180.0 / Math.PI) * 3600.0;
				if (((asteroidPositions.FitQuality == 0) | (asteroidPositions.FitQuality == 5) | (asteroidPositions.FitQuality == 6) | (asteroidPositions.RefYear < 10)) || YearEnd < YearStart)
				{
					continue;
				}
				int.TryParse(asteroidPositions.AsteroidNumber, out var result11);
				if ((AsteroidNumber_only > 0 && AsteroidNumber_only != result11) || (YearStart > 0 && ((YearEnd < asteroidPositions.RefYear) | (YearStart > asteroidPositions.RefYear))) || num > LastJD)
				{
					continue;
				}
				flag = (Gaia_DR2_Updates ? ("12".Contains(asteroidPositions.GaiaVersion) & (text.Trim().Length > 0) & (asteroidPositions.Year_Conj > 10)) : ((asteroidPositions.RefYear > 10) & (text.Trim().Length == 0 || !WithNames)));
				asteroidPositions.NumSatellites = num2;
				bool flag3 = false;
				bool flag4 = false;
				if (asteroidPositions.AsteroidNumber.StartsWith("P9M") && (Asteroids || flag4))
				{
					flag4 = false;
					if (asteroidPositions.AsteroidNumber.Substring(4, 1) != "0")
					{
						asteroidPositions.AsteroidID = "Pluto" + asteroidPositions.AsteroidNumber.Substring(4, 1) + "=" + asteroidPositions.AsteroidID;
						flag4 = true;
					}
					else
					{
						asteroidPositions.AsteroidNumber = " 143430";
					}
				}
				flag2 = false;
				if ((asteroidPositions.AsteroidNumber.StartsWith("P") & (asteroidPositions.AsteroidNumber.Length > 4)) && asteroidPositions.AsteroidNumber.Substring(2, 1) == "M")
				{
					int.TryParse(asteroidPositions.AsteroidNumber.Substring(3, 2), out var result12);
					flag3 = result12 == 0;
					flag4 = result12 > 0;
					flag2 = asteroidPositions.AsteroidNumber.Substring(1, 1) == "4" && flag4;
				}
				bool flag5 = false;
				flag5 = asteroidPositions.AsteroidNumber.EndsWith("P") | asteroidPositions.AsteroidNumber.EndsWith("I") | asteroidPositions.AsteroidNumber.Contains("P/") | asteroidPositions.AsteroidNumber.Contains("C/") | asteroidPositions.AsteroidNumber.Contains("A/");
				if (flag)
				{
					flag = ((Asteroids && ((!flag3 && !flag2) || (flag4 && !flag2) || flag5)) | (AsteroidSatellites & (asteroidPositions.NumSatellites > 0 || flag2))) || (Planets && flag3);
				}
				if (IncludeAlreadyReported)
				{
					flag = true;
				}
				if (flag)
				{
					flag = !(ExcludeStarsWithNoPM & (asteroidPositions.NoGaiaPM == 1));
				}
				if ((Asteroids && !Planets) & asteroidPositions.AsteroidNumber.Contains("143430"))
				{
					flag = false;
				}
				if (!flag)
				{
					continue;
				}
				bool flag6 = true;
				int num16 = 0;
				if (Observer.Length > 0)
				{
					flag6 = false;
					for (int l = 0; l < OccEvents[i].Lines.Count; l++)
					{
						if (OccEvents[i].Lines[l].ToString().Contains(Tags.TagStart[21].ToString()))
						{
							if (OccEvents[i].Lines[l].ToString().Trim().Replace(Tags.TagStart[21], "")
								.Replace(Tags.TagEnd[21], "")
								.Replace("&apos;", "'")
								.Split(new char[1] { '|' })[1].Trim().Replace(". ", " ").Replace(".", " ")
								.Trim()
								.ToLower()
								.Contains(Observer))
							{
								flag6 = true;
							}
							else if (ObserverOnly)
							{
								num16++;
							}
						}
					}
				}
				if (ObserverOnly && num16 > 0)
				{
					flag6 = false;
				}
				if (!flag6)
				{
					continue;
				}
				Asteroid_Observations_Reports.PositionsList.Add(asteroidPositions);
				for (int m = 0; m < OccEvents[i].Lines.Count; m++)
				{
					if (OccEvents[i].Lines[m].ToString().Contains(Tags.TagStart[21].ToString()))
					{
						string[] array8 = OccEvents[i].Lines[m].ToString().Trim().Replace(Tags.TagStart[21], "")
							.Replace(Tags.TagEnd[21], "")
							.Replace("&apos;", "'")
							.Split(new char[1] { '|' });
						MPCName mPCName = new MPCName();
						mPCName.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(array8[1].Trim().Replace(". ", " ").Replace(".", " ")).Trim());
						MPCName mPCName2 = mPCName;
						string[] obj = new string[9]
						{
							asteroidPositions.AsteroidNumber,
							" ",
							asteroidPositions.AsteroidID,
							" ",
							asteroidPositions.Year_Conj.ToString(),
							" ",
							asteroidPositions.Month_Conj.ToString(),
							" ",
							null
						};
						GeocentricY_atConj = asteroidPositions.Day_Conj;
						obj[8] = GeocentricY_atConj.ToString();
						mPCName2.EventID = string.Concat(obj);
						double.TryParse(array8[6].Substring(0, 4).Replace("-", "").Replace("+", ""), out result);
						if (array8[6].Substring(0, 4).Contains("-"))
						{
							result = 0.0 - result;
						}
						double.TryParse(array8[7].Substring(0, 3).Replace("-", "").Replace("+", ""), out result2);
						if (array8[7].Substring(0, 3).Contains("-"))
						{
							result2 = 0.0 - result2;
						}
						mPCName.Region = ObserverRegion(result, result2);
						Asteroid_Observations_Reports.ObserverList.Add(mPCName);
						if (array8[2].Trim().Length > 0)
						{
							mPCName = new MPCName();
							mPCName.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(array8[2].Trim().Replace(". ", " ").Replace(".", " ")).Trim());
							MPCName mPCName3 = mPCName;
							string[] obj2 = new string[9]
							{
								asteroidPositions.AsteroidNumber,
								" ",
								asteroidPositions.AsteroidID,
								" ",
								asteroidPositions.Year_Conj.ToString(),
								" ",
								asteroidPositions.Month_Conj.ToString(),
								" ",
								null
							};
							GeocentricY_atConj = asteroidPositions.Day_Conj;
							obj2[8] = GeocentricY_atConj.ToString();
							mPCName3.EventID = string.Concat(obj2);
							mPCName.Region = ObserverRegion(result, result2);
							Asteroid_Observations_Reports.ObserverList.Add(mPCName);
						}
					}
				}
			}
			((Form)pBar).Close();
		}

		internal static int ObserverRegion(double Longitude, double Latitude)
		{
			if (stats.Count < 6)
			{
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
			}
			for (int j = 0; j < 6; j++)
			{
				if ((Longitude > (double)Stat[j].LongWest) & (Longitude < (double)Stat[j].LongEast) & (Latitude < (double)Stat[j].LatNorth) & (Latitude > (double)Stat[j].LatSouth))
				{
					return j;
				}
			}
			return -1;
		}

		internal string GetAsteroidEvents_NoMainBody()
		{
			string text = "";
			string AsteroidNumber = "";
			string AsteroidName = "";
			int num = OccEvents.Count - 1;
			int Year = 0;
			int Month = 0;
			int Day = 0;
			int num2 = 0;
			for (int i = 0; i <= num; i++)
			{
				bool flag = false;
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[23]) && OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
						.Split(new char[1] { '|' })[1] == "e")
					{
						flag = true;
						break;
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[24]) && OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[24], "").Replace(Tags.TagEnd[24], "")
						.Split(new char[1] { '|' })[1] == "f")
					{
						flag = true;
						break;
					}
				}
				if (flag)
				{
					GetAsteroidID(i, out AsteroidNumber, out AsteroidName);
					GetEventDate(i, out Year, out Month, out Day);
					text = text + Year + " " + Utilities.ShortMonths[Month] + " " + Day.ToString().PadLeft(2, '0') + AsteroidNumber.PadLeft(9) + " " + AsteroidName + "\r\n";
					num2++;
					if (num2 % 5 == 0)
					{
						text += "\r\n";
					}
				}
			}
			return text;
		}

		internal string Get_InvalidCountryCodes()
		{
			return "";
		}

		internal string GetUnfittedEvents(out int Count)
		{
			string text = "";
			Count = 0;
			int num = 0;
			for (int i = 0; i < OccEvents.Count; i++)
			{
				string text2 = "";
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[1]))
					{
						string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						text2 = array[0] + array[1].PadLeft(3) + array[2].PadLeft(3) + "   ";
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
					{
						string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						text2 = text2 + array2[0].PadLeft(7) + " " + array2[1].PadRight(16);
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[6]) && OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
						.Split(new char[1] { '|' })[5] == "0")
					{
						Count++;
						text = text + text2 + "\r\n";
						num++;
						if (num % 5 == 0)
						{
							text += "\r\n";
						}
					}
				}
			}
			return text;
		}

		internal string GetEventsForReview(out int Count)
		{
			string text = "";
			Count = 0;
			int num = 0;
			for (int i = 0; i < OccEvents.Count; i++)
			{
				string text2 = "";
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[1]))
					{
						string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						text2 = array[0] + array[1].PadLeft(3) + array[2].PadLeft(3) + "   ";
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
					{
						string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						text2 = text2 + array2[0].PadLeft(7) + " " + array2[1].PadRight(18);
					}
					if (!OccEvents[i].Lines[j].Contains(Tags.TagStart[6]))
					{
						continue;
					}
					string[] array3 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
						.Split(new char[1] { '|' });
					if (array3.Length <= 7 || !int.TryParse(array3[7], out var result) || result <= 0)
					{
						continue;
					}
					Count++;
					string text3 = "";
					for (int k = 1; k < Tags.chkReview_Labels.Length; k++)
					{
						int num2 = (int)Math.Pow(2.0, k - 1);
						if ((result & num2) == num2)
						{
							if (text3.Length > 0)
							{
								text3 += ", ";
							}
							text3 += Tags.chkReview_Labels[k];
						}
					}
					text = text + text2 + text3 + "\r\n";
					num++;
					if (num % 5 == 0)
					{
						text += "\r\n";
					}
				}
			}
			if (num == 0)
			{
				text = "No events for review have been found";
			}
			return text;
		}

		internal string GetBadUncertainties(out double UncertaintyRatio)
		{
			string text = "";
			UncertaintyRatio = 0.52;
			int num = 0;
			for (int i = 0; i < OccEvents.Count; i++)
			{
				string text2 = "";
				double result = 0.0;
				double result2 = 0.0;
				double num2 = 0.0;
				double result3 = 0.0;
				double result4 = 1.0;
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[1]))
					{
						string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
							.Split(new char[1] { '|' });
						text2 = array[0] + array[1].PadLeft(3) + array[2].PadLeft(3);
					}
					if (OccEvents[i].Lines[j].Contains(Tags.TagStart[4]))
					{
						string[] array2 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
							.Split(new char[1] { '|' });
						if (!double.TryParse(array2[8], out result4))
						{
							result4 = 0.1;
						}
						if (array2.Length == 11)
						{
							double.TryParse(array2[9], out result3);
						}
						else if (array2.Length == 12)
						{
							double.TryParse(array2[10], out result3);
						}
						else
						{
							double.TryParse(array2[10], out result3);
						}
						num2 = result3 / 6378.137 * result4;
						text2 += array2[0].PadLeft(9);
					}
					if (!OccEvents[i].Lines[j].Contains(Tags.TagStart[13]))
					{
						continue;
					}
					string[] array3 = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[13], "").Replace(Tags.TagEnd[13], "")
						.Split(new char[1] { '|' });
					double.TryParse(array3[5], out result);
					double.TryParse(array3[6], out result2);
					if ((result > UncertaintyRatio * num2) | (result2 > UncertaintyRatio * num2))
					{
						text = text + text2 + string.Format("  {0,4:f3}   {1,4:f3}   {2,4:f3}", num2, result, result2) + "\r\n";
						num++;
						if (num % 5 == 0)
						{
							text += "\r\n";
						}
					}
				}
			}
			return text;
		}

		internal int SearchFreeText(string SearchString, out List<string> Events)
		{
			Events = new List<string>();
			int num = 0;
			string value = SearchString.ToLower();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < OccEvents.Count; i++)
			{
				bool flag = false;
				for (int j = 0; j < OccEvents[i].Lines.Count; j++)
				{
					if (!OccEvents[i].Lines[j].Contains(Tags.TagStart[22]))
					{
						continue;
					}
					string[] array = OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[22], "").Replace(Tags.TagEnd[22], "")
						.Split(new char[1] { '|' });
					if (array[4].ToLower().Contains(value))
					{
						GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
						string starID = GetStarID(i);
						GetEventDate(i, out var _, out var _, out var _);
						string item = AsteroidNumber.PadLeft(7) + " " + AsteroidName.PadRight(18) + GetFormattedEventDate(i) + "    " + starID + "\r\n";
						if (!flag)
						{
							Events.Add(item);
						}
						if (OccEvents[i].Lines[j - 1].Contains(Tags.TagStart[21]))
						{
							string[] array2 = OccEvents[i].Lines[j - 1].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
								.Split(new char[1] { '|' });
							Events.Add("#" + array2[0].PadLeft(3) + "  " + array[4]);
						}
						else
						{
							Events.Add("#..?  " + array[4]);
						}
						flag = true;
						num++;
					}
				}
				if (flag)
				{
					Events.Add("");
				}
			}
			return num;
		}

		internal double Get_MidTime_of_Observations_in_Editor()
		{
			double num = 0.0;
			int num2 = 0;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if ("DdGg".Contains(EventDetails.Observers[i].Event_D) && EventDetails.Observers[i].T_Disappear != 0.0)
				{
					num += EventDetails.Observers[i].T_Disappear;
					num2++;
				}
				if ("RrBb".Contains(EventDetails.Observers[i].Event_R) && EventDetails.Observers[i].T_Reappear != 0.0)
				{
					num += EventDetails.Observers[i].T_Reappear;
					num2++;
				}
			}
			if (num2 == 0)
			{
				return 12.0;
			}
			return num / (double)num2;
		}

		internal string SaveFileName(int EventNumber)
		{
			GetEventDate(EventNumber, out var Year, out var Month, out var Day);
			string eventID = GetEventID(EventNumber, IncludeMagnitude: false, Kepler2: false);
			return eventID.Substring(0, 6).Trim() + "_" + eventID.Substring(7, 13).Trim().Replace(" ", "") + "_" + Year + Utilities.ShortMonths[Month] + Day.ToString().PadLeft(2, '0');
		}

		internal void EncodeAnEvent_inXML(bool NewEvent, int EventNumber)
		{
			if (EventNumber > 0)
			{
				EventDetails.YearEdited = DateTime.Now.ToUniversalTime().Year;
				EventDetails.MonthEdited = DateTime.Now.ToUniversalTime().Month;
				EventDetails.DayEdited = DateTime.Now.ToUniversalTime().Day;
				if (NewEvent)
				{
					EventDetails.YearAdded = DateTime.Now.ToUniversalTime().Year;
					EventDetails.MonthAdded = DateTime.Now.ToUniversalTime().Month;
					EventDetails.DayAdded = DateTime.Now.ToUniversalTime().Day;
				}
			}
			LinesForAnEvent linesForAnEvent = new LinesForAnEvent();
			linesForAnEvent.EventJD = Utilities.JD_from_Date(EventDetails.Year, EventDetails.Month, (double)EventDetails.Day + EventDetails.MidT_forMotions / 24.0);
			string text = " ";
			int num = 0;
			if (int.TryParse(EventDetails.AsteroidNumber, out var _))
			{
				DisplayMPOccultations.IsInISAM(EventDetails.AsteroidNo, out var NumModels);
				DisplayMPOccultations.IsInDAMIT(EventDetails.AsteroidNo, out var NumModels2);
				num = NumModels2 + NumModels;
				if (num > 0)
				{
					if (EventDetails.ShapeData.Count >= num)
					{
						text = "#";
					}
					else if (EventDetails.ShapeData.Count < num)
					{
						text = "†";
					}
				}
			}
			int num2 = 0;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (!EventDetails.Observers[i].Observer1.ToUpper().Contains("PREDICT"))
				{
					num2++;
				}
			}
			linesForAnEvent.IndexLine = EventDetails.Year + EventDetails.Month.ToString().PadLeft(3) + EventDetails.Day.ToString().PadLeft(3) + text + EventDetails.AsteroidNumber.PadLeft(7) + (" " + EventDetails.AsteroidID.PadRight(14)).Substring(0, 14) + num2.ToString().PadLeft(4) + " " + EventDetails.Number_Chords.ToString().PadLeft(3);
			linesForAnEvent.Lines = XMLforAnEvent();
			linesForAnEvent.IsXML = true;
			if (NewEvent)
			{
				OccEvents.Add(linesForAnEvent);
			}
			else
			{
				OccEvents[EventNumber] = linesForAnEvent;
			}
		}

		internal List<string> XMLforAnEvent()
		{
			DecimalPlacesInSolution_Output = DecimalPlaces_String_InSolution_Output(EventDetails.Y_Dia);
			List<string> list = new List<string>();
			list.Add("".PadLeft(Tags.TagIndent[0]) + Tags.TagStart[0]);
			string item = "".PadLeft(Tags.TagIndent[1]) + Tags.TagStart[1] + EventDetails.Year + "|" + EventDetails.Month + "|" + EventDetails.Day + "|" + string.Format("{0,2:F1}", EventDetails.MidT_forMotions) + Tags.TagEnd[1];
			list.Add(item);
			list.Add("".PadLeft(Tags.TagIndent[2]) + Tags.TagStart[2]);
			string item2 = "".PadLeft(Tags.TagIndent[3]) + Tags.TagStart[3] + EventDetails.StarCat.Trim() + "|" + EventDetails.StarNumber.Trim() + "|" + EventDetails.GaiaVersion + "|" + EventDetails.Gaia_ID + "|" + string.Format("{0,12:F10}|", EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0) + string.Format("{0,11:F9}|", EventDetails.Dec_Star_2000 * (180.0 / Math.PI)) + string.Format("{0,2:F2}|", EventDetails.RA_Star_Uncertainty_mas) + string.Format("{0,2:F2}|", EventDetails.Dec_Star_Uncertainty_mas) + string.Format("{0,2:F2}|", EventDetails.StarDia_mas) + string.Format("{0,1:F0}|", EventDetails.IssuesFlag) + string.Format("{0,7:F8}|", EventDetails.RA_Star_Apparent * (180.0 / Math.PI) / 15.0) + string.Format("{0,7:F7}|", EventDetails.Dec_Star_Apparent * (180.0 / Math.PI)) + string.Format("{0,2:F2}|", EventDetails.MbStar) + string.Format("{0,2:F2}|", EventDetails.MgStar) + string.Format("{0,2:F2}|", EventDetails.MrStar) + EventDetails.Kepler2ID + Tags.TagEnd[3];
			list.Add(item2);
			string item3 = "".PadLeft(Tags.TagIndent[34]) + Tags.TagStart[34] + string.Format("{0,2:f2}|", EventDetails.StarReliability) + EventDetails.DuplicatedSource + "|" + EventDetails.NoGaiaPM + "|" + EventDetails.GaiaPMfromUCAC4 + "|" + string.Format("{0,2:f2}|", EventDetails.BrightnessRatio_SolnStar_to_Companion) + string.Format("{0,2:f0}|", EventDetails.BrightnessRatio_UncertaintyPercent) + string.Format("{0,2:f2}|", EventDetails.RA_Offset_DoubleStar_mas) + string.Format("{0,2:f2}|", EventDetails.Dec_Offset_DoubleStar_mas) + string.Format("{0,2:f2}|", EventDetails.RA_Offset_DoubleStar_sDev_mas) + string.Format("{0,2:f2}|", EventDetails.Dec_Offset_DoubleStar_sDev_mas) + EventDetails.KnownPair_ID + Tags.TagEnd[34];
			list.Add(item3);
			string text = EventDetails.AsteroidID;
			if (text.Trim() == "G!kun||'homdima")
			{
				text = "Gunhoumdima";
			}
			string item4 = "".PadLeft(Tags.TagIndent[4]) + Tags.TagStart[4] + EventDetails.AsteroidNumber.Trim() + "|" + Utilities.HTML_EncodeString(text.Trim()) + "|" + string.Format("{0,2:F8}", EventDetails.dX) + "|" + string.Format("{0,2:F8}", EventDetails.dY) + "|" + string.Format("{0,2:F7}", EventDetails.d2X) + "|" + string.Format("{0,2:F7}", EventDetails.d2Y) + "|" + string.Format("{0,2:F7}", EventDetails.d3X) + "|" + string.Format("{0,2:F7}", EventDetails.d3Y) + "|" + string.Format("{0,2:F5}", EventDetails.Parallax) + "|" + string.Format("{0,2:F5}", EventDetails.dParallax) + "|" + string.Format("{0,2:F1}", EventDetails.AsteroidNominalDiameter) + "|" + string.Format("{0,2:F1}", EventDetails.DiameterUncertainty) + "|" + string.Format("{0,2:F2}", EventDetails.MvAsteroid) + Tags.TagEnd[4];
			list.Add(item4);
			list.Add("".PadLeft(Tags.TagIndent[32]) + Tags.TagStart[32]);
			string item5 = "".PadLeft(Tags.TagIndent[5]) + Tags.TagStart[5] + Convert.ToInt32(EventDetails.Solve_X) + "|" + Convert.ToInt32(EventDetails.Solve_Y) + "|" + Convert.ToInt32(EventDetails.Solve_Major) + "|" + Convert.ToInt32(EventDetails.Solve_Minor) + "|" + Convert.ToInt32(EventDetails.Solve_PA) + "|" + Convert.ToInt32(EventDetails.Solve_Circular) + "|" + Convert.ToInt32(EventDetails.Inc_Miss) + "|" + Convert.ToInt32(EventDetails.Solve_CompanionSep) + "|" + Convert.ToInt32(EventDetails.Solve_CompanionPA) + Tags.TagEnd[5];
			list.Add(item5);
			if ((EventDetails.CentreOfMass_Offset_X != 0.0) | (EventDetails.CentreOfMass_Offset_Y != 0.0))
			{
				EventDetails.AstrometryShapeModelCentered = false;
			}
			string item6 = "".PadLeft(Tags.TagIndent[6]) + Tags.TagStart[6] + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.X) + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.Y) + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.X_Dia) + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.Y_Dia) + "|" + string.Format("{0,2:F1}", EventDetails.PA_Ellipse) + "|" + EventDetails.Quality + "|" + Convert.ToInt32(EventDetails.UsedAssumedDiameter) + "|" + EventDetails.FlagForReview + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.CentreOfMass_Offset_X) + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.CentreOfMass_Offset_Y) + Tags.TagEnd[6];
			list.Add(item6);
			string text2 = "".PadLeft(Tags.TagIndent[7]) + Tags.TagStart[7];
			text2 = ((!EventDetails.Sdev_X_Set) ? (text2 + "|") : (text2 + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_X)));
			text2 = ((!EventDetails.Sdev_Y_Set) ? (text2 + "|") : (text2 + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_Y)));
			text2 = ((!EventDetails.Sdev_Major_Set) ? (text2 + "|") : (text2 + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_Major)));
			text2 = ((!EventDetails.Sdev_Minor_Set) ? (text2 + "|") : (text2 + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_Minor)));
			if (EventDetails.Sdev_PA_Ellipse_Set)
			{
				text2 += string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}", EventDetails.Sdev_PA_Ellipse);
			}
			text2 += Tags.TagEnd[7];
			list.Add(text2);
			if (EventDetails.ShapeData.Count > 0)
			{
				list.Add("".PadLeft(Tags.TagIndent[8]) + Tags.TagStart[8]);
				for (int i = 0; i < EventDetails.ShapeData.Count; i++)
				{
					list.Add(ShapeModelLine(i));
				}
				list.Add("".PadLeft(Tags.TagIndent[8]) + Tags.TagEnd[8]);
			}
			if (EventDetails.AsteroidHasSatellite = EventDetails.NumberOfSatellites > 0)
			{
				list.Add("".PadLeft(Tags.TagIndent[30]) + Tags.TagStart[30]);
				for (int j = 0; j < EventDetails.NumberOfSatellites; j++)
				{
					string item7 = "".PadLeft(Tags.TagIndent[10]) + Tags.TagStart[10] + SatelliteLine(j) + Tags.TagEnd[10];
					list.Add(item7);
				}
				list.Add("".PadLeft(Tags.TagIndent[30]) + Tags.TagEnd[30]);
			}
			if (EventDetails.StarIsDouble & (EventDetails.Doubles.Count > 0))
			{
				list.Add("".PadLeft(Tags.TagIndent[16]) + Tags.TagStart[16]);
				list.Add("".PadLeft(Tags.TagIndent[35]) + Tags.TagStart[35] + EventDetails.JDSO_SubmitDate + "|" + EventDetails.JDSO_Vol_Num_Pg + Tags.TagEnd[35]);
				for (int k = 0; k < 4; k++)
				{
					if (EventDetails.Doubles[k].Sep_Companion != 0.0)
					{
						list.Add(DoubleStarLine(k));
					}
				}
				list.Add("".PadLeft(Tags.TagIndent[16]) + Tags.TagEnd[16]);
			}
			list.Add("".PadLeft(Tags.TagIndent[32]) + Tags.TagEnd[32]);
			if (EventDetails.Quality > 0)
			{
				list.Add("".PadLeft(Tags.TagIndent[11]) + Tags.TagStart[11]);
				string item8 = "".PadLeft(Tags.TagIndent[12]) + Tags.TagStart[12] + EventDetails.Year + "|" + EventDetails.Month + "|" + EventDetails.Day + "|" + string.Format("{0,2:F7}", EventDetails.RefHour_forAnalysis) + "|" + string.Format("{0,2:F3}", EventDetails.RefHour_forAnalysis_Uncert_secs) + "|" + string.Format("{0,2:F2}", EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas) + Tags.TagEnd[12];
				list.Add(item8);
				string item9 = "".PadLeft(Tags.TagIndent[13]) + Tags.TagStart[13] + EventDetails.AsteroidNumber.Trim() + string.Format("|{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.X_Geo_atEvent) + string.Format("|{0,2:F" + DecimalPlacesInSolution_Output + "}", EventDetails.Y_Geo_atEvent) + string.Format("|{0,2:F4}", EventDetails.dRACosDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.dDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.Sdev_dRACosDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.Sdev_dDec_atEvent) + "|" + Convert.ToInt32(EventDetails.AstrometryShapeModelCentered) + "|" + EventDetails.Number_Chords + "|" + Convert.ToInt32(EventDetails.UnseenBinaryPrimary) + "|" + EventDetails.FitUncertaintyCategory + string.Format("|{0,1:F1}", EventDetails.AdjustmentTo_sDev_AlongTrack) + string.Format("|{0,1:F1}", EventDetails.AdjustmentTo_sDev_AcrossTrack) + string.Format("|{0,1:F2}", EventDetails.MaxHitPlus) + string.Format("|{0,1:F2}", EventDetails.MaxHitMinus) + string.Format("|{0,1:F2}", EventDetails.MinMissPlus) + string.Format("|{0,1:F2}", EventDetails.MinMissMinus) + Tags.TagEnd[13];
				list.Add(item9);
				string item10 = "".PadLeft(Tags.TagIndent[29]) + Tags.TagStart[29] + EventDetails.AsteroidNumber.Trim() + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.X_Geo_atConj) + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Y_Geo_atConj) + EventDetails.Year_Conj + "|" + EventDetails.Month_Conj + "|" + string.Format("{0,2:F7}|", EventDetails.Day_Conj) + string.Format("{0,2:F7}|", EventDetails.Sdev_T_Conj) + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_AlongTrack) + string.Format("{0,2:F4}|", EventDetails.Sep_Conj) + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Sdev_Sep_Conj) + string.Format("{0,2:F3}", EventDetails.PA_Conj_2000) + "|" + Convert.ToInt32(EventDetails.UnseenBinaryPrimary) + Tags.TagEnd[29];
				list.Add(item10);
				if (EventDetails.AsteroidHasSatellite & (EventDetails.Satellites.Count > 0))
				{
					list.Add("".PadLeft(Tags.TagIndent[33]) + Tags.TagStart[33]);
					for (int l = 0; l < EventDetails.NumberOfSatellites; l++)
					{
						string item11 = "".PadLeft(Tags.TagIndent[14]) + Tags.TagStart[14] + SatelliteAstrometryLine(l) + Tags.TagEnd[14];
						list.Add(item11);
						string item12 = "".PadLeft(Tags.TagIndent[31]) + Tags.TagStart[31] + SatelliteConjunctionAstrometryLine(l) + Tags.TagEnd[31];
						list.Add(item12);
					}
					list.Add("".PadLeft(Tags.TagIndent[33]) + Tags.TagEnd[33]);
				}
				string item13 = "".PadLeft(Tags.TagIndent[15]) + Tags.TagStart[15] + EventDetails.MPCDate.Trim() + "|" + EventDetails.MPCNumber.Trim() + "|" + EventDetails.MPCsubmissionID + Tags.TagEnd[15];
				list.Add(item13);
				list.Add("".PadLeft(Tags.TagIndent[11]) + Tags.TagEnd[11]);
			}
			list.Add("".PadLeft(Tags.TagIndent[2]) + Tags.TagEnd[2]);
			if (EventDetails.Observers.Count > 0)
			{
				list.Add("".PadLeft(Tags.TagIndent[18]) + Tags.TagStart[18]);
				for (int m = 0; m < EventDetails.Observers.Count; m++)
				{
					if (EventDetails.Observers[m].Observer1.ToUpper().Contains("PREDICT"))
					{
						string item14 = "".PadLeft(Tags.TagIndent[19]) + Tags.TagStart[19] + EventDetails.Observers[m].SeqNumber + "|" + EventDetails.Observers[m].FormattedLongitude + "|" + EventDetails.Observers[m].FormattedLatitude + "|" + EventDetails.Observers[m].Formatted_T_Disappear + "|" + Utilities.HTML_EncodeString(EventDetails.Observers[m].FreeText.Trim()) + Tags.TagEnd[19];
						list.Add(item14);
						continue;
					}
					list.Add("".PadLeft(Tags.TagIndent[20]) + Tags.TagStart[20]);
					string text3 = "".PadLeft(Tags.TagIndent[21]) + Tags.TagStart[21] + EventDetails.Observers[m].SeqNumber + "|" + Utilities.ProperCase(Utilities.HTML_EncodeString(EventDetails.Observers[m].Observer1.Trim())) + "|" + Utilities.ProperCase(Utilities.HTML_EncodeString(EventDetails.Observers[m].Observer2.Trim())) + "|" + Convert.ToInt32(EventDetails.Observers[m].MoreThan2Observers) + "|" + EventDetails.Observers[m].NearTo.Trim() + "|" + EventDetails.Observers[m].StateCountry + "|" + EventDetails.Observers[m].FormattedLongitude + "|" + EventDetails.Observers[m].FormattedLatitude + "|" + EventDetails.Observers[m].Altitude + "|" + EventDetails.Observers[m].Datum.ToString() + "|";
					text3 = ((!EventDetails.Observers[m].TelescopeAperture_Set) ? (text3 + " ") : (text3 + EventDetails.Observers[m].TelescopeAperture));
					text3 = text3 + "|" + EventDetails.Observers[m].TelescopeType + "|" + EventDetails.Observers[m].Method + "|" + EventDetails.Observers[m].TimeSource + Tags.TagEnd[21];
					list.Add(text3);
					string text4 = "".PadLeft(Tags.TagIndent[22]) + Tags.TagStart[22] + EventDetails.Observers[m].Stability.Trim() + "|" + EventDetails.Observers[m].Transparency.Trim() + "|";
					text4 = ((!EventDetails.Observers[m].SNR_Set) ? (text4 + "|") : (text4 + string.Format("{0,1:f1}|", EventDetails.Observers[m].SNR)));
					text4 = ((!EventDetails.Observers[m].TimeAdjustment_Set) ? (text4 + "|") : (text4 + string.Format("{0,1:f2}|", EventDetails.Observers[m].TimeAdjustment)));
					text4 = text4 + EventDetails.Observers[m].FreeText.Trim().Replace("&", "&amp;").Replace("<", "&lt;")
						.Replace(">", "&gt;")
						.Replace("\"", "&quot;")
						.Replace("'", "&apos;") + Tags.TagEnd[22];
					list.Add(text4);
					string text5 = "".PadLeft(Tags.TagIndent[23]) + Tags.TagStart[23];
					text5 = ((EventDetails.Observers[m].T_Disappear == 0.0) ? (text5 + "        . |") : (text5 + EventDetails.Observers[m].H_Dis.ToString().PadLeft(2) + " " + EventDetails.Observers[m].M_Dis.ToString().PadLeft(2) + " " + Utilities.FormatNumber(EventDetails.Observers[m].S_Dis, 2, EventDetails.Observers[m].S_Dis_DecPlaces) + "|"));
					text5 = text5 + EventDetails.Observers[m].Event_D + "|";
					text5 = ((!EventDetails.Observers[m].Accuracy_D_Set) ? (text5 + "|") : (text5 + Utilities.FormatNumber(EventDetails.Observers[m].Accuracy_D, 1, EventDetails.Observers[m].Accuracy_D_DecPlaces) + "|"));
					text5 = ((!EventDetails.Observers[m].PEQ_D_Set) ? (text5 + "|") : (text5 + Utilities.FormatNumber(EventDetails.Observers[m].PEQ_D, 1, EventDetails.Observers[m].PEQ_D_N) + "|"));
					text5 = ((!EventDetails.Observers[m].Weight_D_Set) ? (text5 + "|") : (text5 + EventDetails.Observers[m].Weight_D + "|"));
					text5 = text5 + EventDetails.Observers[m].PlotCode + Tags.TagEnd[23];
					list.Add(text5);
					string text6 = "".PadLeft(Tags.TagIndent[24]) + Tags.TagStart[24];
					text6 = ((EventDetails.Observers[m].T_Reappear == 0.0) ? (text6 + "        . |") : (text6 + EventDetails.Observers[m].H_Reap.ToString().PadLeft(2) + " " + EventDetails.Observers[m].M_Reap.ToString().PadLeft(2) + " " + Utilities.FormatNumber(EventDetails.Observers[m].S_Reap, 2, EventDetails.Observers[m].S_Reap_DecPlaces) + "|"));
					text6 = text6 + EventDetails.Observers[m].Event_R + "|";
					text6 = ((!EventDetails.Observers[m].Accuracy_R_Set) ? (text6 + "|") : (text6 + Utilities.FormatNumber(EventDetails.Observers[m].Accuracy_R, 1, EventDetails.Observers[m].Accuracy_R_DecPlaces) + "|"));
					text6 = ((!EventDetails.Observers[m].PEQ_R_Set) ? (text6 + "|") : (text6 + Utilities.FormatNumber(EventDetails.Observers[m].PEQ_R, 1, EventDetails.Observers[m].PEQ_R_N) + "|"));
					text6 = ((!EventDetails.Observers[m].Weight_R_Set) ? (text6 + "|") : (text6 + EventDetails.Observers[m].Weight_R + "|"));
					text6 = text6 + EventDetails.Observers[m].PlotCode + Tags.TagEnd[24];
					list.Add(text6);
					list.Add("".PadLeft(Tags.TagIndent[20]) + Tags.TagEnd[20]);
				}
				list.Add("".PadLeft(Tags.TagIndent[18]) + Tags.TagEnd[18]);
				list.Add("".PadLeft(Tags.TagIndent[25]) + Tags.TagStart[25] + EventDetails.YearAdded + "|" + EventDetails.MonthAdded + "|" + EventDetails.DayAdded + Tags.TagEnd[25]);
				list.Add("".PadLeft(Tags.TagIndent[26]) + Tags.TagStart[26] + EventDetails.YearEdited + "|" + EventDetails.MonthEdited + "|" + EventDetails.DayEdited + Tags.TagEnd[26]);
			}
			list.Add("".PadLeft(Tags.TagIndent[0]) + Tags.TagEnd[0]);
			return list;
		}

		internal string SatelliteLine(int k)
		{
			return EventDetails.Satellites[k].CompanionIAUname + "|" + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_dRA_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_dDec_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_d2RA_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_d2Dec_mas) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].SatelliteSeparation) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].SatellitePA_Apparent) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].Sat_Sep_Uncertainty) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].Sat_PA_Uncertainty) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].MajorAxisSatellite) + string.Format("{0,1:F3}|", EventDetails.Satellites[k].MinorAxisSatellite) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].PAAxisSatellite) + EventDetails.Satellites[k].SatelliteQuality + "|" + EventDetails.Satellites[k].CBET_Discovery;
		}

		internal string SatelliteAstrometryLine(int k)
		{
			return EventDetails.Satellites[k].CompanionIAUname + "|" + string.Format("{0,2:F3}|", EventDetails.Satellites[k].XSat_Geo_atEvent) + string.Format("{0,2:F3}|", EventDetails.Satellites[k].YSat_Geo_atEvent) + string.Format("{0,2:F1}|", EventDetails.Satellites[k].SatellitePA_2000) + string.Format("{0,2:F4}|", EventDetails.Satellites[k].dRACosDecSat_atEvent) + string.Format("{0,2:F4}|", EventDetails.Satellites[k].ddecSat_atEvent) + string.Format("{0,2:F4}|", EventDetails.Satellites[k].Sdev_dRACosDecSat_atEvent) + string.Format("{0,2:F4}|", EventDetails.Satellites[k].Sdev_dDecSat_atEvent) + EventDetails.Satellites[k].NumberOfChords;
		}

		internal string SatelliteConjunctionAstrometryLine(int k)
		{
			string text = "0";
			if (EventDetails.Satellites[k].SatelliteMotionIncluded)
			{
				text = "1";
			}
			return EventDetails.Satellites[k].CompanionIAUname + "|" + text + "|" + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Satellites[k].XSat_Geo_atConj) + string.Format("{0,2:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Satellites[k].YSat_Geo_atConj) + EventDetails.Satellites[k].Year_Conj + "|" + EventDetails.Satellites[k].Month_Conj + "|" + string.Format("{0,2:F7}|", EventDetails.Satellites[k].Day_Conj) + string.Format("{0,2:F7}|", EventDetails.Satellites[k].Sdev_T_Conj) + string.Format("{0,2:F1}|", EventDetails.Satellites[k].Sdev_AlongTrack) + string.Format("{0,2:F4}|", EventDetails.Satellites[k].Sep_Conj_Star) + string.Format("{0,2:F1}|", EventDetails.Satellites[k].Sdev_Sep_Conj) + string.Format("{0,2:F3}", EventDetails.Satellites[k].PA_Conj_2000_Star);
		}

		internal string ShapeModelLine(int i)
		{
			string text = "".PadLeft(Tags.TagIndent[9]) + Tags.TagStart[9] + EventDetails.ShapeData[i].Source + "|" + EventDetails.ShapeData[i].ID + "|" + string.Format("{0,5:f3}|", EventDetails.ShapeData[i].SurfaceVolumeRatio) + EventDetails.ShapeData[i].FitQuality + "|" + EventDetails.ShapeData[i].PhaseCorrn + "|";
			text = ((EventDetails.ShapeData[i].DiaMin > 30.0) ? (text + string.Format("{0,2:f0}|", EventDetails.ShapeData[i].DiaMin) + string.Format("{0,2:f0}|", EventDetails.ShapeData[i].DiaMax)) : ((!(EventDetails.ShapeData[i].DiaMin > 3.0)) ? (text + string.Format("{0,2:f2}|", EventDetails.ShapeData[i].DiaMin) + string.Format("{0,2:f2}|", EventDetails.ShapeData[i].DiaMax)) : (text + string.Format("{0,2:f1}|", EventDetails.ShapeData[i].DiaMin) + string.Format("{0,2:f1}|", EventDetails.ShapeData[i].DiaMax))));
			return text + Utilities.HTML_EncodeString(EventDetails.ShapeData[i].Version) + Tags.TagEnd[9];
		}

		internal string DoubleStarLine(int i)
		{
			string text = "".PadLeft(Tags.TagIndent[17]) + Tags.TagStart[17] + string.Format("{0,1:F1}|", EventDetails.Doubles[i].PA_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sep_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sdev_PA_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sdev_Sep_Companion);
			text = ((!((EventDetails.Doubles[2].Sep_Companion > 0.0) | (EventDetails.Doubles[3].Sep_Companion > 0.0))) ? (text + "0|0|0|0|") : (text + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Doubles[i].Centre_X) + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Doubles[i].Centre_Y) + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Doubles[i].Offset_X) + string.Format("{0,1:F" + DecimalPlacesInSolution_Output + "}|", EventDetails.Doubles[i].Offset_Y)));
			return text + EventDetails.Doubles[i].SolutionID + Tags.TagEnd[17];
		}

		internal void Parse_EventDate(string XMLline)
		{
			Parse_EventDate(XMLline, out var Year, out var Month, out var Day, out var MidT);
			EventDetails.Year = Year;
			EventDetails.Month = Month;
			EventDetails.Day = Day;
			EventDetails.MidT_forMotions = MidT;
		}

		internal static void Parse_EventDate(string XMLline, out int Year, out int Month, out int Day, out double MidT)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[0], out var result);
			Year = result;
			int.TryParse(array[1], out result);
			Month = result;
			int.TryParse(array[2], out result);
			Day = result;
			double.TryParse(array[3], out var result2);
			MidT = result2;
		}

		internal void Parse_Star(string XMLline, out double RA2000, out double Dec2000, out string Source, out double StarMag, out double StarDia_mas)
		{
			Parse_Star(XMLline, out RA2000, out Dec2000, out Source, out StarMag, out var _, out var _, out StarDia_mas);
		}

		internal void Parse_Star(string XMLline, out double RA2000, out double Dec2000, out string Source, out double StarMag, out string StarCat, out string StarNumber, out double StarDia_mas)
		{
			StarMag = (StarDia_mas = 0.0);
			string[] array = XMLline.Trim().Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "")
				.Split(new char[1] { '|' });
			StarCat = array[0];
			StarNumber = array[1];
			double result;
			if (VersionOfReadFile < 1.5)
			{
				double.TryParse(array[2], out result);
				RA2000 = result * 15.0 / (180.0 / Math.PI);
				double.TryParse(array[3], out result);
				Dec2000 = result / (180.0 / Math.PI);
				Source = array[2];
				if (!double.TryParse(array[7], out StarMag))
				{
					StarMag = 20.0;
				}
				return;
			}
			double.TryParse(array[4], out result);
			RA2000 = result * 15.0 / (180.0 / Math.PI);
			double.TryParse(array[5], out result);
			Dec2000 = result / (180.0 / Math.PI);
			double.TryParse(array[8], out result);
			StarDia_mas = result;
			Source = array[2];
			if (!double.TryParse(array[13], out StarMag))
			{
				StarMag = 20.0;
			}
		}

		internal void Parse_Star(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[3], "").Replace(Tags.TagEnd[3], "")
				.Split(new char[1] { '|' });
			int result;
			double result2;
			long result3;
			if (VersionOfReadFile < 1.5)
			{
				EventDetails.StarCat = array[0];
				EventDetails.StarNumber = array[1];
				int.TryParse(array[2], out result);
				EventDetails.GaiaVersion = result;
				double.TryParse(array[3], out result2);
				EventDetails.RA_Star_2000 = result2 * 15.0 / (180.0 / Math.PI);
				double.TryParse(array[4], out result2);
				EventDetails.Dec_Star_2000 = result2 / (180.0 / Math.PI);
				double.TryParse(array[5], out result2);
				EventDetails.RA_Star_Apparent = result2 * 15.0 / (180.0 / Math.PI);
				double.TryParse(array[6], out result2);
				EventDetails.Dec_Star_Apparent = result2 / (180.0 / Math.PI);
				double.TryParse(array[7], out result2);
				EventDetails.MgStar = result2;
				double.TryParse(array[8], out result2);
				EventDetails.MrStar = result2;
				long.TryParse(array[9], out result3);
				EventDetails.Kepler2ID = result3;
				EventDetails.Gaia_ID = "";
				EventDetails.RA_Star_Uncertainty_mas = 0.0;
				EventDetails.Dec_Star_Uncertainty_mas = 0.0;
				EventDetails.StarDia_mas = 0.0;
				EventDetails.IssuesFlag = 0;
				EventDetails.MbStar = 25.0;
			}
			else
			{
				EventDetails.StarCat = array[0];
				EventDetails.StarNumber = array[1];
				int.TryParse(array[2], out result);
				EventDetails.GaiaVersion = result;
				EventDetails.Gaia_ID = array[3];
				double.TryParse(array[4], out result2);
				EventDetails.RA_Star_2000 = result2 * 15.0 / (180.0 / Math.PI);
				double.TryParse(array[5], out result2);
				EventDetails.Dec_Star_2000 = result2 / (180.0 / Math.PI);
				double.TryParse(array[6], out result2);
				EventDetails.RA_Star_Uncertainty_mas = result2;
				double.TryParse(array[7], out result2);
				EventDetails.Dec_Star_Uncertainty_mas = result2;
				double.TryParse(array[8], out result2);
				EventDetails.StarDia_mas = result2;
				int.TryParse(array[9], out result);
				EventDetails.IssuesFlag = result;
				double.TryParse(array[10], out result2);
				EventDetails.RA_Star_Apparent = result2 * 15.0 / (180.0 / Math.PI);
				double.TryParse(array[11], out result2);
				EventDetails.Dec_Star_Apparent = result2 / (180.0 / Math.PI);
				double.TryParse(array[12], out result2);
				EventDetails.MbStar = result2;
				double.TryParse(array[13], out result2);
				EventDetails.MgStar = result2;
				double.TryParse(array[14], out result2);
				EventDetails.MrStar = result2;
				long.TryParse(array[15], out result3);
				EventDetails.Kepler2ID = result3;
			}
		}

		internal void Parse_StarIssues(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[34], "").Replace(Tags.TagEnd[34], "")
				.Split(new char[1] { '|' });
			double.TryParse(array[0], out var result);
			EventDetails.StarReliability = result;
			int.TryParse(array[1], out var result2);
			EventDetails.DuplicatedSource = result2;
			int.TryParse(array[2], out result2);
			EventDetails.NoGaiaPM = result2;
			int.TryParse(array[3], out result2);
			EventDetails.GaiaPMfromUCAC4 = result2;
			if (array.Length >= 10)
			{
				double.TryParse(array[4], out result);
				EventDetails.BrightnessRatio_SolnStar_to_Companion = result;
				int.TryParse(array[5], out result2);
				EventDetails.BrightnessRatio_UncertaintyPercent = result2;
				double.TryParse(array[6], out result);
				EventDetails.RA_Offset_DoubleStar_mas = result;
				double.TryParse(array[7], out result);
				EventDetails.Dec_Offset_DoubleStar_mas = result;
				double.TryParse(array[8], out result);
				EventDetails.RA_Offset_DoubleStar_sDev_mas = result;
				double.TryParse(array[9], out result);
				EventDetails.Dec_Offset_DoubleStar_sDev_mas = result;
				try
				{
					EventDetails.KnownPair_ID = array[10];
				}
				catch
				{
					EventDetails.KnownPair_ID = "";
				}
			}
			else
			{
				EventDetails.BrightnessRatio_SolnStar_to_Companion = 1.2;
				EventDetails.BrightnessRatio_UncertaintyPercent = 10;
				EventDetails.RA_Offset_DoubleStar_mas = (EventDetails.Dec_Offset_DoubleStar_mas = (EventDetails.RA_Offset_DoubleStar_sDev_mas = (EventDetails.Dec_Offset_DoubleStar_sDev_mas = 0.0)));
			}
			if (EventDetails.BrightnessRatio_UncertaintyPercent < 10)
			{
				EventDetails.BrightnessRatio_UncertaintyPercent = 10;
			}
		}

		internal void Parse_Asteroid(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "")
				.Split(new char[1] { '|' });
			EventDetails.AsteroidNumber = array[0];
			EventDetails.AsteroidID = Utilities.HTML_DecodeString(array[1]);
			if (EventDetails.AsteroidID == "Gunhoumdima")
			{
				EventDetails.AsteroidID = "G!kun||'homdima";
			}
			double.TryParse(array[2], out var result);
			EventDetails.dX = result;
			double.TryParse(array[3], out result);
			EventDetails.dY = result;
			double.TryParse(array[4], out result);
			EventDetails.d2X = result;
			double.TryParse(array[5], out result);
			EventDetails.d2Y = result;
			double.TryParse(array[6], out result);
			EventDetails.d3X = result;
			double.TryParse(array[7], out result);
			EventDetails.d3Y = result;
			double.TryParse(array[8], out result);
			EventDetails.Parallax = result;
			if (array.Length == 11)
			{
				EventDetails.dParallax = 0.0;
				double.TryParse(array[9], out result);
				EventDetails.AsteroidNominalDiameter = result;
				EventDetails.DiameterUncertainty = EventDetails.AsteroidNominalDiameter / 10.0;
				double.TryParse(array[10], out result);
				EventDetails.MvAsteroid = result;
			}
			else if (array.Length == 12)
			{
				double.TryParse(array[9], out result);
				EventDetails.dParallax = result;
				double.TryParse(array[10], out result);
				EventDetails.AsteroidNominalDiameter = result;
				EventDetails.DiameterUncertainty = EventDetails.AsteroidNominalDiameter / 10.0;
				double.TryParse(array[11], out result);
				EventDetails.MvAsteroid = result;
			}
			else if (array.Length == 13)
			{
				double.TryParse(array[9], out result);
				EventDetails.dParallax = result;
				double.TryParse(array[10], out result);
				EventDetails.AsteroidNominalDiameter = result;
				double.TryParse(array[11], out result);
				EventDetails.DiameterUncertainty = result;
				if (EventDetails.DiameterUncertainty == 0.0)
				{
					EventDetails.DiameterUncertainty = EventDetails.AsteroidNominalDiameter / 10.0;
				}
				double.TryParse(array[12], out result);
				EventDetails.MvAsteroid = result;
			}
		}

		internal void Parse_SolveFlags(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[5], "").Replace(Tags.TagEnd[5], "")
				.Split(new char[1] { '|' });
			EventDetails.Solve_X = array[0] == "1";
			EventDetails.Solve_Y = array[1] == "1";
			EventDetails.Solve_Major = array[2] == "1";
			EventDetails.Solve_Minor = array[3] == "1";
			EventDetails.Solve_PA = array[4] == "1";
			EventDetails.Solve_Circular = array[5] == "1";
			EventDetails.Inc_Miss = array[6] == "1";
			EventDetails.Solve_CompanionSep = array[7] == "1";
			EventDetails.Solve_CompanionPA = array[8] == "1";
		}

		internal static void Parse_SolveFlags(string XMLline, out bool Solve_X, out bool Solve_Y, out bool Solve_Major, out bool Solve_Minor, out bool Solve_PA, out bool Solve_Circular, out bool Inc_Miss, out bool Solve_CompanionSep, out bool Solve_CompanionPA)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[5], "").Replace(Tags.TagEnd[5], "")
				.Split(new char[1] { '|' });
			Solve_X = array[0] == "1";
			Solve_Y = array[1] == "1";
			Solve_Major = array[2] == "1";
			Solve_Minor = array[3] == "1";
			Solve_PA = array[4] == "1";
			Solve_Circular = array[5] == "1";
			Inc_Miss = array[6] == "1";
			Solve_CompanionSep = array[7] == "1";
			Solve_CompanionPA = array[8] == "1";
		}

		internal void Parse_EllipticFit(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
				.Split(new char[1] { '|' });
			double.TryParse(array[0], out var result);
			EventDetails.X = result;
			double.TryParse(array[1], out result);
			EventDetails.Y = result;
			double.TryParse(array[2], out result);
			EventDetails.X_Dia = result;
			double.TryParse(array[3], out result);
			EventDetails.Y_Dia = result;
			double.TryParse(array[4], out result);
			EventDetails.PA_Ellipse = result;
			int.TryParse(array[5], out var result2);
			EventDetails.Quality = result2;
			EventDetails.UsedAssumedDiameter = (array[6] == "1") | (EventDetails.X_Dia == 0.0) | (EventDetails.Y_Dia == 0.0);
			if (array.Length > 7)
			{
				int.TryParse(array[7], out result2);
				EventDetails.FlagForReview = result2;
			}
			else
			{
				EventDetails.FlagForReview = 0;
			}
			if (array.Length > 8)
			{
				double.TryParse(array[8], out result);
				EventDetails.CentreOfMass_Offset_X = result;
				double.TryParse(array[9], out result);
				EventDetails.CentreOfMass_Offset_Y = result;
			}
			else
			{
				EventDetails.CentreOfMass_Offset_X = (EventDetails.CentreOfMass_Offset_Y = 0.0);
			}
		}

		internal static void Parse_EllipticFit(string XMLline, out string X, out string Y, out string X_Dia, out string Y_Dia, out string PA_Ellipse, out string Quality, out bool UsedAssumedDiameter, out bool FlagForReview, out double CentreOfMass_Offset_X, out double CentreOfMass_Offset_Y)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
				.Split(new char[1] { '|' });
			X = array[0];
			Y = array[1];
			X_Dia = array[2];
			Y_Dia = array[3];
			PA_Ellipse = array[4];
			Quality = array[5];
			UsedAssumedDiameter = (array[6] == "1") | (X_Dia == "0") | (Y_Dia == "0");
			FlagForReview = false;
			if (array.Length > 7)
			{
				FlagForReview = array[7] == "1";
			}
			if (array.Length > 8)
			{
				double.TryParse(array[8], out CentreOfMass_Offset_X);
				double.TryParse(array[9], out CentreOfMass_Offset_Y);
			}
			else
			{
				CentreOfMass_Offset_X = (CentreOfMass_Offset_Y = 0.0);
			}
		}

		internal static void Parse_EllipticFit(string XMLline, out int FitQuality)
		{
			int.TryParse(XMLline.Trim().Replace(Tags.TagStart[6], "").Replace(Tags.TagEnd[6], "")
				.Split(new char[1] { '|' })[5], out FitQuality);
		}

		internal void Parse_EllipticUncertainty(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[7], "").Replace(Tags.TagEnd[7], "")
				.Split(new char[1] { '|' });
			EventDetails.Sdev_X_Set = double.TryParse(array[0], out var result);
			EventDetails.Sdev_X = result;
			EventDetails.Sdev_Y_Set = double.TryParse(array[1], out result);
			EventDetails.Sdev_Y = result;
			EventDetails.Sdev_Major_Set = double.TryParse(array[2], out result);
			EventDetails.Sdev_Major = result;
			EventDetails.Sdev_Minor_Set = double.TryParse(array[3], out result);
			EventDetails.Sdev_Minor = result;
			EventDetails.Sdev_PA_Ellipse_Set = double.TryParse(array[4], out result);
			EventDetails.Sdev_PA_Ellipse = result;
		}

		internal static void Parse_EllipticUncertainty(string XMLline, out string eX, out string eY, out string eMajor, out string eMinor, out string ePA)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[7], "").Replace(Tags.TagEnd[7], "")
				.Split(new char[1] { '|' });
			eX = array[0];
			eY = array[1];
			eMajor = array[2];
			eMinor = array[3];
			ePA = array[4];
		}

		internal void Parse_ShapeModelFit(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[9], "").Replace(Tags.TagEnd[9], "")
				.Split(new char[1] { '|' });
			ShapeModelData shapeModelData = new ShapeModelData();
			shapeModelData.Source = array[0];
			shapeModelData.ID = array[1];
			double.TryParse(array[2], out var result);
			shapeModelData.SurfaceVolumeRatio = result;
			shapeModelData.FitQuality = int.Parse(array[3]);
			int.TryParse(array[4], out var result2);
			shapeModelData.PhaseCorrn = result2;
			double.TryParse(array[5], out result);
			shapeModelData.DiaMin = result;
			double.TryParse(array[6], out result);
			shapeModelData.DiaMax = result;
			shapeModelData.Version = Utilities.HTML_DecodeString(array[7]);
			EventDetails.ShapeData.Add(shapeModelData);
			EventDetails.ShapeModelFitted = true;
		}

		internal static void Parse_ShapeModelFit(string XMLline, out string Summary)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[9], "").Replace(Tags.TagEnd[9], "")
				.Split(new char[1] { '|' });
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(array[0].PadRight(5) + "#" + array[1].PadRight(6));
			stringBuilder.AppendFormat(" {0,4:F0}km <> {1,4:f0}km,  Phs{2,4:f0}°, Q = {3,1:f0}", array[5], array[6], array[4], array[3]);
			Summary = stringBuilder.ToString();
		}

		internal void Parse_Satellite(string XMLline, int SatelliteCount)
		{
			EventDetails.AsteroidHasSatellite = true;
			string[] array = XMLline.Trim().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
				.Split(new char[1] { '|' });
			SatelliteData satelliteData = new SatelliteData();
			satelliteData.CompanionIAUname = array[0];
			if (satelliteData.CompanionIAUname == "IAU designation")
			{
				satelliteData.CompanionIAUname = "";
			}
			double.TryParse(array[1], out var result);
			satelliteData.Sat_dRA_mas = result;
			double.TryParse(array[2], out result);
			satelliteData.Sat_dDec_mas = result;
			double.TryParse(array[3], out result);
			satelliteData.Sat_d2RA_mas = result;
			double.TryParse(array[4], out result);
			satelliteData.Sat_d2Dec_mas = result;
			double.TryParse(array[5], out result);
			satelliteData.SatelliteSeparation = result;
			double.TryParse(array[6], out result);
			satelliteData.SatellitePA_Apparent = result;
			double.TryParse(array[7], out result);
			satelliteData.Sat_Sep_Uncertainty = result;
			double.TryParse(array[8], out result);
			satelliteData.Sat_PA_Uncertainty = result;
			double.TryParse(array[9], out result);
			satelliteData.MajorAxisSatellite = result;
			double.TryParse(array[10], out result);
			satelliteData.MinorAxisSatellite = result;
			double.TryParse(array[11], out result);
			satelliteData.PAAxisSatellite = result;
			int.TryParse(array[12], out var result2);
			satelliteData.SatelliteQuality = result2;
			if (array.Length > 13)
			{
				satelliteData.CBET_Discovery = array[13];
			}
			else
			{
				satelliteData.CBET_Discovery = "";
			}
			EventDetails.Satellites[SatelliteCount] = satelliteData;
			SatelliteCount++;
			EventDetails.NumberOfSatellites = SatelliteCount;
		}

		internal static void Parse_Satellite(string XMLline, out string Summary, out double Separation_mas, out double PA_deg)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
				.Split(new char[1] { '|' });
			Summary = array[5].PadLeft(6) + "mas" + array[6].PadLeft(11) + "°, " + array[9].PadLeft(5) + " x " + array[10].PadLeft(5) + "km" + array[11].PadLeft(6) + "°, " + array[0];
			double.TryParse(array[5], out Separation_mas);
			double.TryParse(array[6], out PA_deg);
		}

		internal static void Parse_Satellite(string XMLline, out int FitQuality)
		{
			int.TryParse(XMLline.Trim().Replace(Tags.TagStart[10], "").Replace(Tags.TagEnd[10], "")
				.Split(new char[1] { '|' })[12], out FitQuality);
		}

		internal void Parse_ReferenceTime(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[12], "").Replace(Tags.TagEnd[12], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[0], out var result);
			EventDetails.Year = result;
			int.TryParse(array[1], out result);
			EventDetails.Month = result;
			int.TryParse(array[2], out result);
			EventDetails.Day = result;
			double.TryParse(array[3], out var result2);
			EventDetails.RefHour_forAnalysis = result2;
			if (VersionOfReadFile >= 2.0)
			{
				double.TryParse(array[4], out result2);
				EventDetails.RefHour_forAnalysis_Uncert_secs = result2;
			}
			else
			{
				EventDetails.RefHour_forAnalysis_Uncert_secs = 0.0;
			}
			if (array.Length > 5)
			{
				double.TryParse(array[5], out result2);
				EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas = result2;
			}
			else
			{
				EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas = 0.0;
			}
		}

		internal void Parse_ReferenceTime(string XMLline, out int Year, out int Month, out int Day, out double Hour, out double RefHour_forAnalysis_Uncert_secs, out double RefHour_forAnalysis_AcrossPathUncertainty_mas)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[12], "").Replace(Tags.TagEnd[12], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[0], out Year);
			int.TryParse(array[1], out Month);
			int.TryParse(array[2], out Day);
			double.TryParse(array[3], out Hour);
			if (VersionOfReadFile >= 2.0)
			{
				double.TryParse(array[4], out RefHour_forAnalysis_Uncert_secs);
			}
			else
			{
				RefHour_forAnalysis_Uncert_secs = 0.0;
			}
			if (array.Length > 5)
			{
				double.TryParse(array[5], out RefHour_forAnalysis_AcrossPathUncertainty_mas);
			}
			else
			{
				RefHour_forAnalysis_AcrossPathUncertainty_mas = 0.0;
			}
		}

		internal void Parse_MainBodyLine(string XMLline)
		{
			string[] array = ((!XMLline.Contains(Tags.TagStart[13])) ? XMLline.Trim().Replace(Tags.TagStart_Old[0], "").Replace(Tags.TagEnd_Old[0], "")
				.Split(new char[1] { '|' }) : XMLline.Trim().Replace(Tags.TagStart[13], "").Replace(Tags.TagEnd[13], "")
				.Split(new char[1] { '|' }));
			double.TryParse(array[1], out var result);
			EventDetails.X_Geo_atEvent = result;
			double.TryParse(array[2], out result);
			EventDetails.Y_Geo_atEvent = result;
			double.TryParse(array[3], out result);
			EventDetails.dRACosDec_atEvent = result;
			double.TryParse(array[4], out result);
			EventDetails.dDec_atEvent = result;
			double.TryParse(array[5], out result);
			EventDetails.Sdev_dRACosDec_atEvent = result;
			double.TryParse(array[6], out result);
			EventDetails.Sdev_dDec_atEvent = result;
			EventDetails.AstrometryShapeModelCentered = array[7] == "1";
			int.TryParse(array[8], out var result2);
			EventDetails.Number_Chords = result2;
			if (array.Length > 9)
			{
				EventDetails.UnseenBinaryPrimary = array[9] == "1";
			}
			else
			{
				EventDetails.UnseenBinaryPrimary = false;
			}
			if (array.Length > 12)
			{
				EventDetails.FitUncertaintyCategory = array[10];
				double.TryParse(array[11], out result);
				EventDetails.AdjustmentTo_sDev_AcrossTrack = result;
				double.TryParse(array[12], out result);
				EventDetails.AdjustmentTo_sDev_AlongTrack = result;
				double.TryParse(array[13], out result);
				EventDetails.MaxHitPlus = result;
				double.TryParse(array[14], out result);
				EventDetails.MaxHitMinus = result;
				double.TryParse(array[15], out result);
				EventDetails.MinMissPlus = result;
				double.TryParse(array[16], out result);
				EventDetails.MinMissMinus = result;
			}
			else
			{
				EventDetails.FitUncertaintyCategory = "";
				EventDetails.AdjustmentTo_sDev_AcrossTrack = (EventDetails.AdjustmentTo_sDev_AlongTrack = 0.0);
			}
		}

		internal void Parse_MainBodyLine(string XMLline, out string IAU_ID, out double X, out double Y, out double dRACosDec_atEvent, out double dDec_atEvent, out double Sdev_dRACosDec_atEvent, out double Sdev_dDec_atEvent, out bool AstrometryShapeModelCentered, out int Number_Chords, out bool UnseenBinaryPrimary, out string FitCode)
		{
			string[] array = ((!XMLline.Contains(Tags.TagStart[13])) ? XMLline.Trim().Replace(Tags.TagStart_Old[0], "").Replace(Tags.TagEnd_Old[0], "")
				.Split(new char[1] { '|' }) : XMLline.Trim().Replace(Tags.TagStart[13], "").Replace(Tags.TagEnd[13], "")
				.Split(new char[1] { '|' }));
			IAU_ID = array[0];
			double.TryParse(array[1], out X);
			double.TryParse(array[2], out Y);
			double.TryParse(array[3], out dRACosDec_atEvent);
			double.TryParse(array[4], out dDec_atEvent);
			double.TryParse(array[5], out Sdev_dRACosDec_atEvent);
			double.TryParse(array[6], out Sdev_dDec_atEvent);
			AstrometryShapeModelCentered = array[7] == "1";
			int.TryParse(array[8], out Number_Chords);
			if (array.Length > 9)
			{
				UnseenBinaryPrimary = array[9] == "1";
			}
			else
			{
				UnseenBinaryPrimary = false;
			}
			if (array.Length > 10)
			{
				FitCode = array[10];
			}
			else
			{
				FitCode = "";
			}
		}

		internal void Parse_AtConjunction(string XMLline)
		{
			Parse_AtConjunction(XMLline, out var _, out var GeocentricX_atConj, out var GeocentricY_atConj, out var Year_Conj, out var Month_Conj, out var Day_Conj, out var SDev_Day_Conj, out var sDev_AlongTrack, out var SeparationAtConj, out var sDevSeparationAtConj, out var PA_Conj, out var UnseenBinaryPrimary);
			EventDetails.X_Geo_atConj = GeocentricX_atConj;
			EventDetails.Y_Geo_atConj = GeocentricY_atConj;
			EventDetails.Year_Conj = Year_Conj;
			EventDetails.Month_Conj = Month_Conj;
			EventDetails.Day_Conj = Day_Conj;
			EventDetails.Sdev_T_Conj = SDev_Day_Conj;
			EventDetails.Sdev_AlongTrack = sDev_AlongTrack;
			EventDetails.Sep_Conj = SeparationAtConj;
			EventDetails.Sdev_Sep_Conj = sDevSeparationAtConj;
			EventDetails.PA_Conj_2000 = PA_Conj;
			EventDetails.UnseenBinaryPrimary = UnseenBinaryPrimary;
		}

		internal void Parse_AtConjunction(string XMLline, out string IAU_ID, out double GeocentricX_atConj, out double GeocentricY_atConj, out int Year_Conj, out int Month_Conj, out double Day_Conj, out double SDev_Day_Conj, out double sDev_AlongTrack, out double SeparationAtConj, out double sDevSeparationAtConj, out double PA_Conj, out bool UnseenBinaryPrimary)
		{
			string[] array = ((!XMLline.Contains(Tags.TagStart[29])) ? XMLline.Trim().Replace(Tags.TagStart_Old[1], "").Replace(Tags.TagEnd_Old[1], "")
				.Split(new char[1] { '|' }) : XMLline.Trim().Replace(Tags.TagStart[29], "").Replace(Tags.TagEnd[29], "")
				.Split(new char[1] { '|' }));
			IAU_ID = array[0];
			double.TryParse(array[1], out GeocentricX_atConj);
			double.TryParse(array[2], out GeocentricY_atConj);
			int.TryParse(array[3], out Year_Conj);
			int.TryParse(array[4], out Month_Conj);
			double.TryParse(array[5], out Day_Conj);
			double.TryParse(array[6], out SDev_Day_Conj);
			double.TryParse(array[7], out sDev_AlongTrack);
			double.TryParse(array[8], out SeparationAtConj);
			double.TryParse(array[9], out sDevSeparationAtConj);
			double.TryParse(array[10], out PA_Conj);
			if (array.Length > 11)
			{
				UnseenBinaryPrimary = array[11] == "1";
			}
			else
			{
				UnseenBinaryPrimary = false;
			}
		}

		internal void Parse_SecondaryLine(string XMLline, out string IAU_ID, out double GeocentricX_atEvent, out double GeocentricY_atEvent, out double dRACosDec_Sat_atEvent, out double dDec_Sat_atEvent, out double Sdev_dRACosDec_Sat_atEvent, out double Sdev_dDec_Sat_atEvent, out int Number_Chords_Sat)
		{
			int num = 0;
			string[] array = XMLline.Trim().Replace(Tags.TagStart[14], "").Replace(Tags.TagEnd[14], "")
				.Split(new char[1] { '|' });
			if (array.Length == 9)
			{
				num = 1;
			}
			IAU_ID = array[0];
			double.TryParse(array[1], out GeocentricX_atEvent);
			double.TryParse(array[2], out GeocentricY_atEvent);
			double.TryParse(array[3 + num], out dRACosDec_Sat_atEvent);
			double.TryParse(array[4 + num], out dDec_Sat_atEvent);
			double.TryParse(array[5 + num], out Sdev_dRACosDec_Sat_atEvent);
			double.TryParse(array[6 + num], out Sdev_dDec_Sat_atEvent);
			int.TryParse(array[7 + num], out Number_Chords_Sat);
		}

		internal void Parse_SecondaryLine(string XMLline, int SatNum)
		{
			int num = 0;
			string[] array = XMLline.Trim().Replace(Tags.TagStart[14], "").Replace(Tags.TagEnd[14], "")
				.Split(new char[1] { '|' });
			SatelliteData satelliteData = EventDetails.Satellites[SatNum];
			double.TryParse(array[1], out var result);
			satelliteData.XSat_Geo_atEvent = result;
			double.TryParse(array[2], out result);
			satelliteData.YSat_Geo_atEvent = result;
			if (array.Length == 9)
			{
				double.TryParse(array[3], out result);
				satelliteData.SatellitePA_2000 = result;
				double num2 = Utilities.PrecessionalRotationToJ2000_deg(EventDetails.JD_EventDate, EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent);
				satelliteData.SatellitePA_Apparent = satelliteData.SatellitePA_2000 - num2;
				num = 1;
			}
			double.TryParse(array[3 + num], out result);
			satelliteData.dRACosDecSat_atEvent = result;
			double.TryParse(array[4 + num], out result);
			satelliteData.ddecSat_atEvent = result;
			double.TryParse(array[5 + num], out result);
			satelliteData.Sdev_dRACosDecSat_atEvent = result;
			double.TryParse(array[6 + num], out result);
			satelliteData.Sdev_dDecSat_atEvent = result;
			int.TryParse(array[7 + num], out var result2);
			satelliteData.NumberOfChords = result2;
			EventDetails.Satellites[SatNum] = satelliteData;
		}

		internal void Parse_Secondary_AtConjunction(string XMLline, out int Year_Conj, out int Month_Conj, out double Day_Conj, out double Sdev_T_Conj, out double Sdev_AlongTrack, out double Sep_Conj_Star, out double sDev_Sep_Conj, out double PA_Conj_Star)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[31], "").Replace(Tags.TagEnd[31], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[4], out Year_Conj);
			int.TryParse(array[5], out Month_Conj);
			double.TryParse(array[6], out Day_Conj);
			double.TryParse(array[7], out Sdev_T_Conj);
			double.TryParse(array[8], out Sdev_AlongTrack);
			double.TryParse(array[9], out Sep_Conj_Star);
			double.TryParse(array[10], out sDev_Sep_Conj);
			double.TryParse(array[11], out PA_Conj_Star);
		}

		internal void Parse_SecondaryAtConjunction(string XMLline, int SatNum)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[31], "").Replace(Tags.TagEnd[31], "")
				.Split(new char[1] { '|' });
			EventDetails.Satellites[SatNum].SatelliteMotionIncluded = array[1].Trim() == "1";
			double.TryParse(array[2], out var result);
			EventDetails.Satellites[SatNum].XSat_Geo_atConj = result;
			double.TryParse(array[3], out result);
			EventDetails.Satellites[SatNum].YSat_Geo_atConj = result;
			int.TryParse(array[4], out var result2);
			EventDetails.Satellites[SatNum].Year_Conj = result2;
			int.TryParse(array[5], out result2);
			EventDetails.Satellites[SatNum].Month_Conj = result2;
			double.TryParse(array[6], out result);
			EventDetails.Satellites[SatNum].Day_Conj = result;
			double.TryParse(array[7], out result);
			EventDetails.Satellites[SatNum].Sdev_T_Conj = result;
			double.TryParse(array[8], out result);
			EventDetails.Satellites[SatNum].Sdev_AlongTrack = result;
			double.TryParse(array[9], out result);
			EventDetails.Satellites[SatNum].Sep_Conj_Star = result;
			double.TryParse(array[10], out result);
			EventDetails.Satellites[SatNum].Sdev_Sep_Conj = result;
			double.TryParse(array[11], out result);
			EventDetails.Satellites[SatNum].PA_Conj_2000_Star = result;
		}

		internal void Parse_MPC(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[15], "").Replace(Tags.TagEnd[15], "")
				.Split(new char[1] { '|' });
			EventDetails.MPCDate = array[0];
			EventDetails.MPCNumber = array[1];
			if (array.Length > 2)
			{
				EventDetails.MPCsubmissionID = array[2];
			}
			else
			{
				EventDetails.MPCsubmissionID = "";
			}
		}

		internal void Parse_JDSO(string XMLline)
		{
			string text = XMLline.Trim().Replace(Tags.TagStart[35], "").Replace(Tags.TagEnd[35], "");
			if (!text.Contains("|"))
			{
				EventDetails.JDSO_SubmitDate = (EventDetails.JDSO_Vol_Num_Pg = "");
				return;
			}
			string[] array = text.Split(new char[1] { '|' });
			EventDetails.JDSO_SubmitDate = array[0];
			EventDetails.JDSO_Vol_Num_Pg = array[1];
		}

		internal void Parse_DoubleStarSolution(string XMLline, ref int SolutionCount)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[17], "").Replace(Tags.TagEnd[17], "")
				.Split(new char[1] { '|' });
			DoubleData doubleData = new DoubleData();
			doubleData.PA_Companion = double.Parse(array[0]);
			if (doubleData.PA_Companion < 0.0)
			{
				doubleData.PA_Companion += 360.0;
			}
			doubleData.Sep_Companion = double.Parse(array[1]);
			double.TryParse(array[2], out var result);
			doubleData.Sdev_PA_Companion = result;
			double.TryParse(array[3], out result);
			doubleData.Sdev_Sep_Companion = result;
			double.TryParse(array[4], out result);
			doubleData.Centre_X = result;
			double.TryParse(array[5], out result);
			doubleData.Centre_Y = result;
			double.TryParse(array[6], out result);
			doubleData.Offset_X = result;
			double.TryParse(array[7], out result);
			doubleData.Offset_Y = result;
			if (!int.TryParse(array[8], out var result2))
			{
				result2 = 1;
			}
			doubleData.SolutionID = result2;
			doubleData.Companion_Set = true;
			EventDetails.Doubles[SolutionCount] = doubleData;
			SolutionCount++;
			EventDetails.NumberOfDoubleSolutions = SolutionCount;
			EventDetails.StarIsDouble = true;
		}

		internal static void Parse_DoubleStarSolution(string XMLline, ref int SolutionCount, out string SummaryLine)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[17], "").Replace(Tags.TagEnd[17], "")
				.Split(new char[1] { '|' });
			SolutionCount++;
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("#{0,1:f0} ", SolutionCount);
			stringBuilder.Append(array[1].PadLeft(8) + "  ± " + array[3] + "mas," + array[0].PadLeft(12) + "°  ± " + array[2].PadLeft(4) + "°");
			SummaryLine = stringBuilder.ToString();
		}

		internal void Parse_EventPrediction(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[19], "").Replace(Tags.TagEnd[19], "")
				.Split(new char[1] { '|' });
			ObserverData observerData = new ObserverData();
			observerData.SeqNumber = int.Parse(array[0]);
			observerData.Observer1 = "Predicted";
			string text2 = (observerData.StateCountry = "");
			string text5 = (observerData.Observer2 = (observerData.NearTo = text2));
			observerData.ObserversAll_Create();
			text5 = (observerData.Event_D = (observerData.Event_R = "P"));
			observerData.Longitude_PlusSign = !array[1].Substring(0, 4).Contains("-");
			int.TryParse(array[1].Substring(0, 4).Replace("-", "").Replace("+", ""), out var result);
			observerData.Longitude_Deg = result;
			int.TryParse(array[1].Substring(5, 2), out result);
			observerData.Longitude_Min = result;
			double.TryParse(array[1].Substring(8), out var result2);
			observerData.Longitude_Sec = result2;
			observerData.Longitude_Sec_N = Utilities.DecimalPlaces(array[1].Substring(8));
			observerData.Latitude_PlusSign = !array[2].Substring(0, 3).Contains("-");
			int.TryParse(array[2].Substring(0, 3).Replace("-", "").Replace("+", ""), out result);
			observerData.Latitude_Deg = result;
			int.TryParse(array[2].Substring(4, 2), out result);
			observerData.Latitude_Min = result;
			double.TryParse(array[2].Substring(7), out result2);
			observerData.Latitude_Sec = result2;
			observerData.Latitude_Sec_N = Utilities.DecimalPlaces(array[2].Substring(7));
			int.TryParse(array[3].Substring(0, 2), out result);
			int num3 = (observerData.H_Dis = (observerData.H_Reap = result));
			int.TryParse(array[3].Substring(3, 2), out result);
			num3 = (observerData.M_Dis = (observerData.M_Reap = result));
			double.TryParse(array[3].Substring(6), out result2);
			double num8 = (observerData.S_Dis = (observerData.S_Reap = result2));
			num3 = (observerData.S_Dis_DecPlaces = (observerData.S_Reap_DecPlaces = Utilities.DecimalPlaces(array[3].Substring(6))));
			if (array.Length > 4)
			{
				observerData.FreeText = array[4].Replace("&amp;", "&").Replace("&lt;", "<").Replace("&gt;", ">")
					.Replace("&quot;", "\"")
					.Replace("&apos;", "'");
			}
			else
			{
				observerData.FreeText = "";
			}
			EventDetails.Observers.Add(observerData);
		}

		internal void Parse_Observer(string XMLline1, string XMLline2, string XMLline3, string XMLline4)
		{
			string[] array = XMLline1.Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
				.Split(new char[1] { '|' });
			string[] array2 = XMLline2.Trim().Replace(Tags.TagStart[22], "").Replace(Tags.TagEnd[22], "")
				.Split(new char[1] { '|' });
			string[] array3 = XMLline3.Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
				.Split(new char[1] { '|' });
			string[] array4 = XMLline4.Trim().Replace(Tags.TagStart[24], "").Replace(Tags.TagEnd[24], "")
				.Split(new char[1] { '|' });
			ObserverData observerData = new ObserverData();
			observerData.SeqNumber = int.Parse(array[0]);
			observerData.Observer1 = Utilities.ProperCase(Utilities.HTML_DecodeString(array[1])).Trim().Replace("  ", " ");
			observerData.Observer2 = Utilities.ProperCase(Utilities.HTML_DecodeString(array[2])).Trim().Replace("  ", " ");
			if (observerData.Observer1.Contains("&") & (observerData.Observer2 == ""))
			{
				string[] array5 = observerData.Observer1.Split(new char[1] { '&' });
				observerData.Observer1 = array5[0];
				observerData.Observer2 = array5[1];
			}
			observerData.MoreThan2Observers = array[3] == "1";
			observerData.NearTo = Utilities.ProperCase(array[4]);
			observerData.StateCountry = array[5];
			if (observerData.StateCountry == "UK")
			{
				observerData.StateCountry = "GB";
			}
			observerData.ObserversAll_Create();
			observerData.Longitude_PlusSign = !array[6].Substring(0, 4).Contains("-");
			int.TryParse(array[6].Substring(0, 4).Replace("-", "").Replace("+", ""), out var result);
			observerData.Longitude_Deg = result;
			int.TryParse(array[6].Substring(5, 2), out result);
			observerData.Longitude_Min = result;
			double.TryParse(array[6].Substring(8), out var result2);
			observerData.Longitude_Sec = result2;
			observerData.Longitude_Sec_N = Utilities.DecimalPlaces(array[6].Substring(8));
			if (observerData.Longitude_Sec_N > 1)
			{
				observerData.Longitude_Sec_N = 1;
				observerData.Longitude_Sec = Math.Round(observerData.Longitude_Sec, observerData.Longitude_Sec_N);
				if (observerData.Longitude_Sec > 59.9)
				{
					observerData.Longitude_Sec = 59.9;
				}
			}
			observerData.Latitude_PlusSign = !array[7].Substring(0, 3).Contains("-");
			int.TryParse(array[7].Substring(0, 3).Replace("-", "").Replace("+", ""), out result);
			observerData.Latitude_Deg = result;
			int.TryParse(array[7].Substring(4, 2), out result);
			observerData.Latitude_Min = result;
			double.TryParse(array[7].Substring(7), out result2);
			observerData.Latitude_Sec = result2;
			observerData.Latitude_Sec_N = Utilities.DecimalPlaces(array[7].Substring(7));
			if (observerData.Latitude_Sec_N > 1)
			{
				observerData.Latitude_Sec_N = 1;
				observerData.Latitude_Sec = Math.Round(observerData.Latitude_Sec, observerData.Latitude_Sec_N);
				if (observerData.Latitude_Sec > 59.9)
				{
					observerData.Latitude_Sec = 59.9;
				}
			}
			observerData.Altitude_Set = double.TryParse(array[8], out result2);
			observerData.Altitude = (int)result2;
			observerData.Datum = array[9].PadLeft(1).Substring(0, 1);
			observerData.TelescopeAperture_Set = int.TryParse(array[10], out result);
			observerData.TelescopeAperture = result;
			observerData.TelescopeType = array[11].PadLeft(1);
			observerData.Method = array[12].PadLeft(1);
			observerData.TimeSource = array[13].PadLeft(1);
			observerData.Stability = array2[0].PadLeft(1);
			observerData.Transparency = array2[1].PadLeft(1);
			observerData.SNR_Set = double.TryParse(array2[2], out result2);
			observerData.SNR = result2;
			observerData.TimeAdjustment_Set = double.TryParse(array2[3], out result2);
			observerData.TimeAdjustment = result2;
			observerData.FreeText = Utilities.HTML_DecodeString(array2[4]);
			int num = observerData.FreeText.ToLower().IndexOf("snr");
			if (num >= 0)
			{
				int num2 = observerData.FreeText.IndexOf("=", num) + 1;
				int i;
				for (i = num2; i < observerData.FreeText.Length && ".0123456789".Contains(observerData.FreeText.Substring(i, 1)); i++)
				{
				}
				if (double.TryParse(observerData.FreeText.Substring(num2, i - num2), out var result3))
				{
					observerData.SNR = result3;
					observerData.SNR_Set = true;
					observerData.FreeText = observerData.FreeText.Remove(num, i - num);
				}
				else
				{
					observerData.SNR = 0.0;
					observerData.SNR_Set = false;
				}
			}
			if (observerData.TelescopeType == "8")
			{
				observerData.TimeSource = "c";
			}
			try
			{
				int.TryParse(array3[0].Substring(0, 2), out result);
				observerData.H_Dis = result;
				int.TryParse(array3[0].Substring(3, 2), out result);
				observerData.M_Dis = result;
				double.TryParse(array3[0].Substring(6), out result2);
				observerData.S_Dis = result2;
				observerData.S_Dis_DecPlaces = Utilities.DecimalPlaces(array3[0].Substring(6));
			}
			catch
			{
				observerData.H_Dis = (int)Math.Truncate(EventDetails.MidT_forMotions);
				observerData.M_Dis = (int)(60.0 * (EventDetails.MidT_forMotions - (double)observerData.H_Dis));
				observerData.S_Dis = 0.0;
				observerData.S_Dis_DecPlaces = 0;
			}
			observerData.Event_D = array3[1];
			if (array3[0].Trim() == ".")
			{
				observerData.Accuracy_D_Set = false;
				observerData.Accuracy_D = 0.0;
				observerData.Accuracy_D_DecPlaces = 0;
			}
			else
			{
				observerData.Accuracy_D_Set = double.TryParse(array3[2], out result2);
				observerData.Accuracy_D = result2;
				observerData.Accuracy_D_DecPlaces = Utilities.DecimalPlaces(array3[2]);
			}
			if (observerData.Accuracy_D_Set)
			{
				int num3 = Utilities.SignificantDigitLocation_DecimalNumbers(observerData.Accuracy_D);
				if (num3 > observerData.Accuracy_D_DecPlaces)
				{
					num3 = observerData.Accuracy_D_DecPlaces;
				}
				if (num3 < observerData.Accuracy_D_DecPlaces)
				{
					observerData.Accuracy_D_DecPlaces = num3;
				}
				if (num3 < observerData.S_Dis_DecPlaces)
				{
					observerData.S_Dis_DecPlaces = num3;
				}
				if (observerData.S_Dis_DecPlaces < observerData.Accuracy_D_DecPlaces)
				{
					observerData.S_Dis_DecPlaces = observerData.Accuracy_D_DecPlaces;
				}
				if (double.Parse(string.Format("{0:F" + observerData.S_Dis_DecPlaces + "}", observerData.S_Dis)) >= 60.0)
				{
					observerData.S_Dis = 0.0;
					observerData.M_Dis++;
					if (observerData.M_Dis > 59)
					{
						observerData.M_Dis = 0;
						observerData.H_Dis++;
					}
				}
			}
			observerData.PEQ_D_Set = double.TryParse(array3[3], out result2);
			observerData.PEQ_D = result2;
			observerData.PEQ_D_N = Utilities.DecimalPlaces(array3[3]);
			if (observerData.PEQ_D_N > 2)
			{
				observerData.PEQ_D_N = 2;
			}
			observerData.Weight_D_Set = int.TryParse(array3[4], out result);
			observerData.Weight_D = result;
			if (observerData.Weight_D > 5)
			{
				observerData.Weight_D = 5;
			}
			observerData.PlotCode = array3[5].PadRight(1);
			try
			{
				int.TryParse(array4[0].Substring(0, 2), out result);
				observerData.H_Reap = result;
				int.TryParse(array4[0].Substring(3, 2), out result);
				observerData.M_Reap = result;
				double.TryParse(array4[0].Substring(6), out result2);
				observerData.S_Reap = result2;
				observerData.S_Reap_DecPlaces = Utilities.DecimalPlaces(array4[0].Substring(6));
			}
			catch
			{
				observerData.H_Reap = (int)Math.Truncate(EventDetails.MidT_forMotions);
				observerData.M_Reap = (int)(60.0 * (EventDetails.MidT_forMotions - (double)observerData.H_Reap));
				observerData.S_Reap = 0.0;
				observerData.S_Reap_DecPlaces = 0;
			}
			observerData.Event_R = array4[1];
			if (array4[0].Trim() == ".")
			{
				observerData.Accuracy_R_Set = false;
				observerData.Accuracy_R = 0.0;
				observerData.Accuracy_R_DecPlaces = 0;
			}
			else
			{
				observerData.Accuracy_R_Set = double.TryParse(array4[2], out result2);
				observerData.Accuracy_R = result2;
				observerData.Accuracy_R_DecPlaces = Utilities.DecimalPlaces(array4[2]);
			}
			if (observerData.Accuracy_R_Set)
			{
				int num4 = Utilities.SignificantDigitLocation_DecimalNumbers(observerData.Accuracy_R);
				if (num4 > observerData.Accuracy_R_DecPlaces)
				{
					num4 = observerData.Accuracy_R_DecPlaces;
				}
				if (num4 < observerData.Accuracy_R_DecPlaces)
				{
					observerData.Accuracy_R_DecPlaces = num4;
				}
				if (num4 < observerData.S_Reap_DecPlaces)
				{
					observerData.S_Reap_DecPlaces = num4;
				}
				if (observerData.S_Reap_DecPlaces < observerData.Accuracy_R_DecPlaces)
				{
					observerData.S_Reap_DecPlaces = observerData.Accuracy_R_DecPlaces;
				}
				if (double.Parse(string.Format("{0:F" + observerData.S_Reap_DecPlaces + "}", observerData.S_Reap)) >= 60.0)
				{
					observerData.S_Reap = 0.0;
					observerData.M_Reap++;
					if (observerData.M_Reap > 59)
					{
						observerData.M_Reap = 0;
						observerData.H_Reap++;
					}
				}
			}
			observerData.PEQ_R_Set = double.TryParse(array4[3], out result2);
			observerData.PEQ_R = result2;
			observerData.PEQ_R_N = Utilities.DecimalPlaces(array4[3]);
			if (observerData.PEQ_R_N > 2)
			{
				observerData.PEQ_R_N = 2;
			}
			observerData.Weight_R_Set = int.TryParse(array4[4], out result);
			observerData.Weight_R = result;
			if (observerData.Weight_R > 5)
			{
				observerData.Weight_R = 5;
			}
			observerData.PlotCode = array4[5].PadRight(1);
			if (!((observerData.Event_D == "C") & (observerData.Event_R == "C")))
			{
				EventDetails.Observers.Add(observerData);
			}
		}

		internal void Parse_Added(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[25], "").Replace(Tags.TagEnd[25], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[0], out var result);
			EventDetails.YearAdded = result;
			int.TryParse(array[1], out result);
			EventDetails.MonthAdded = result;
			int.TryParse(array[2], out result);
			EventDetails.DayAdded = result;
		}

		internal void Parse_LastEdited(string XMLline)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[26], "").Replace(Tags.TagEnd[26], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[0], out var result);
			EventDetails.YearEdited = result;
			int.TryParse(array[1], out result);
			EventDetails.MonthEdited = result;
			int.TryParse(array[2], out result);
			EventDetails.DayEdited = result;
		}

		internal static void Parse_LastEdited(string XMLline, out string Date)
		{
			string[] array = XMLline.Trim().Replace(Tags.TagStart[26], "").Replace(Tags.TagEnd[26], "")
				.Split(new char[1] { '|' });
			int.TryParse(array[1], out var result);
			EventDetails.MonthEdited = result;
			Date = array[0] + " " + Utilities.ShortMonths[result] + " " + array[2].PadLeft(2, '0');
		}
	}
}
