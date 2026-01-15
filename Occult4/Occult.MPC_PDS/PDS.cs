using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Properties;

namespace Occult.MPC_PDS
{
	internal class PDS
	{
		internal static string PDSFilePath = Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\";

		internal static string PDSyearPath;

		internal static string PDSHeaderFilePath = Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\Headers\\";

		internal static string AsteroidFile;

		internal static string AsteroidSummaryFile;

		internal static string AsteroidTimesFile;

		internal static string PlanetFile;

		internal static string PlanetSummaryFile;

		internal static string PlanetTimesFile;

		internal static string Asteroid_xyzFile;

		internal static string Planet_xyzFile;

		internal static string Diameters_File;

		internal static string Doubles_File;

		internal static string Satellite_File;

		internal static string EndCoverage = "";

		internal static string TagAsteroid = "[Asteroid]";

		internal static string TagAsteroidSummary = "[AsteroidSummary]";

		internal static string TagAsteroidTimes = "[AsteroidTimes]";

		internal static string TagPlanet = "[Planet]";

		internal static string TagPlanetSummary = "[PlanetSummary]";

		internal static string TagPlanetTimes = "[PlanetTimes]";

		internal static string TagAsteroid_xyz = "[AsteroidAstrometry]";

		internal static string TagPlanet_xyz = "[PlanetAstrometry]";

		internal static string TagDiameters = "[AsteroidDiameters]";

		internal static string TagDoubles = "[DoubleStars]";

		internal static string TagSatellites = "[Satellites]";

		internal static string TagEventCount = "[EventCount]";

		internal static string TagObserverCount = "[ObserverCount]";

		internal static string TagLastDate = "[LastDate]";

		internal static int AsteroidCount = 0;

		internal static int PlanetCount = 0;

		internal static int EventCount = 0;

		internal static int ObserverCount = 0;

		internal static string PDS_Version = "1.0";

		public static void CreateNASA_PDS_Pipe_Files(bool CreateTestFiles)
		{
			MessageForm messageForm = new MessageForm();
			((Control)messageForm).set_Top(20);
			((Control)messageForm).set_Left(150);
			((Control)messageForm).Show();
			((Control)Asteroid_Observations_Reports.MPC_PDS).Focus();
			List<string[]> L = new List<string[]>();
			List<string[]> L2 = new List<string[]>();
			List<string[]> L3 = new List<string[]>();
			List<string[]> L4 = new List<string[]>();
			List<string[]> L5 = new List<string[]>();
			List<string[]> L6 = new List<string[]>();
			List<string[]> L7 = new List<string[]>();
			List<string[]> L8 = new List<string[]>();
			List<string[]> L9 = new List<string[]>();
			FillList(ref L, "HeaderAsteroids");
			FillList(ref L2, "HeaderPlanets");
			FillList(ref L3, "HeaderAsteroidsSummary");
			FillList(ref L4, "HeaderPlanetsSummary");
			FillList(ref L5, "HeaderTimes");
			FillList(ref L6, "HeaderAstrometry");
			FillList(ref L7, "HeaderDiameters");
			FillList(ref L8, "HeaderDoubles");
			FillList(ref L9, "HeaderSatellites");
			string header = GetHeader(ref L);
			string header2 = GetHeader(ref L2);
			string header3 = GetHeader(ref L3);
			string header4 = GetHeader(ref L4);
			string header5 = GetHeader(ref L5);
			string header6 = GetHeader(ref L6);
			string header7 = GetHeader(ref L7);
			string header8 = GetHeader(ref L8);
			string header9 = GetHeader(ref L9);
			string text = "";
			string text2 = "";
			string text3 = "";
			string text4 = "";
			string text5 = "";
			string text6 = "";
			string text7 = "";
			string text8 = "";
			string text9 = "";
			string text10 = "";
			string text11 = "";
			string text12 = "";
			string text13 = "";
			string text14 = "";
			string text15 = "";
			string text16 = "";
			double num = 2415020.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double motion_masSec = 0.0;
			double rmsTimeSecs = 0.0;
			double rmsAcrossPath_fromTime_mas = 0.0;
			string text17 = "";
			string text18 = "";
			string text19 = "";
			bool flag = false;
			string[] array = new string[6];
			string[] array2 = new string[4];
			string text20 = DateTime.Today.Year + Utilities.ShortMonths[DateTime.Today.Month];
			int num4 = 0;
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			int num9 = 0;
			int num10 = 0;
			AsteroidCount = (PlanetCount = (EventCount = (ObserverCount = 0)));
			PDSyearPath = PDSFilePath + "\\" + text20;
			if (!Directory.Exists(PDSyearPath))
			{
				Directory.CreateDirectory(PDSyearPath);
			}
			AsteroidFile = "Asteroid_" + text20;
			AsteroidSummaryFile = "AsteroidSummary_" + text20;
			AsteroidTimesFile = "AsteroidTimes_" + text20;
			PlanetFile = "Planet_" + text20;
			PlanetSummaryFile = "PlanetSummary_" + text20;
			PlanetTimesFile = "PlanetTimes_" + text20;
			Asteroid_xyzFile = "AsteroidAstrometry_" + text20;
			Planet_xyzFile = "PlanetAstrometry_" + text20;
			Diameters_File = "AsteroidDiameters_" + text20;
			Doubles_File = "DoubleStars_" + text20;
			Satellite_File = "Satellites_" + text20;
			string startDate = "1800_01-01";
			string text21 = "2000-01-01";
			string startDate2 = "1800_01-01";
			string endDate = "2000-01-01";
			bool flag2 = false;
			bool flag3 = false;
			((Control)messageForm.label).set_Text("1/6  Creating main PDS files");
			Application.DoEvents();
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile))
			{
				using StreamWriter streamWriter = new StreamWriter(PDSyearPath + "\\" + AsteroidFile + ".psv");
				streamWriter.WriteLine(header);
				using StreamWriter streamWriter2 = new StreamWriter(PDSyearPath + "\\" + AsteroidSummaryFile + ".psv");
				streamWriter2.WriteLine(header3);
				using StreamWriter streamWriter3 = new StreamWriter(PDSyearPath + "\\" + AsteroidTimesFile + ".psv");
				streamWriter3.WriteLine(header5);
				using StreamWriter streamWriter4 = new StreamWriter(PDSyearPath + "\\" + PlanetFile + ".psv");
				streamWriter4.WriteLine(header2);
				using StreamWriter streamWriter5 = new StreamWriter(PDSyearPath + "\\" + PlanetSummaryFile + ".psv");
				streamWriter5.WriteLine(header4);
				using StreamWriter streamWriter6 = new StreamWriter(PDSyearPath + "\\" + PlanetTimesFile + ".psv");
				streamWriter6.WriteLine(header5);
				do
				{
					string text22 = streamReader.ReadLine();
					if (text22.Contains("<Obse") || text22.Contains("<FileV") || !text22.Contains("<Event>"))
					{
						continue;
					}
					for (int i = 0; i < 6; i++)
					{
						array[i] = "||||||||";
					}
					text19 = "||";
					for (int j = 0; j < 4; j++)
					{
						array2[j] = "|||||||||";
					}
					text8 = (text9 = "||||||||||||||");
					text10 = "|||||";
					num = (num2 = (num3 = 0.0));
					text11 = "||||||||||||||||||||";
					text12 = "|||||||||||";
					text13 = (text15 = "||||||||||||");
					text14 = (text16 = "|||||||||||");
					text7 = "||||";
					text17 = "|||";
					text18 = "|||";
					num4 = 0;
					num5 = 0;
					num6 = 0;
					EventCount++;
					do
					{
						text22 = streamReader.ReadLine();
						if (text22.Contains("</Event>"))
						{
							break;
						}
						if (text22.Contains("Details") || text22.Contains("</Astrometry") || text22.Contains("</Double") || text22.Contains("EventFits)"))
						{
							continue;
						}
						if (text22.Contains("<Date>"))
						{
							text = Merge_Y_M_D(StripTags(text22), 0);
							continue;
						}
						if (text22.Contains("<Star>"))
						{
							text2 = StripTags(text22);
							string text23 = StripTags(streamReader.ReadLine());
							string[] array3 = text23.Split(new char[1] { '|' });
							double.TryParse(array3[6], out var result);
							double.TryParse(array3[7], out var result2);
							if (result == 0.0 && result2 == 0.0)
							{
								for (int k = 4; k < array3.Length; k++)
								{
									array3[k] = "";
								}
								text23 = "";
								for (int l = 0; l < array3.Length - 1; l++)
								{
									text23 = text23 + array3[l] + "|";
								}
							}
							text2 += text23;
							string[] array4 = text2.Split(new char[1] { '|' });
							num2 = double.Parse(array4[4]) * 15.0 / (180.0 / Math.PI);
							num3 = double.Parse(array4[5]) / (180.0 / Math.PI);
							text3 = Utilities.HTML_DecodeString(StripTags(streamReader.ReadLine()));
							string[] array5 = text3.Split(new char[1] { '|' });
							double num11 = double.Parse(array5[2]);
							double num12 = double.Parse(array5[3]);
							double num13 = double.Parse(array5[8]);
							motion_masSec = Math.Sqrt(num11 * num11 + num12 * num12) / 3600.0 * num13 * 1000.0;
							flag = array5[0].Trim().StartsWith("P") & !array5[0].Contains("P/");
							string[] array6 = text.Split(new char[1] { '|' });
							if (flag)
							{
								text3 = text3.Replace("P1M", "m").Replace("P2M", "V").Replace("P4M", "M")
									.Replace("P5M", "J")
									.Replace("P6M", "S")
									.Replace("P7M", "U")
									.Replace("P8M", "N")
									.Replace("P9M", "P");
								PlanetCount++;
								endDate = array6[0];
								if (!flag3)
								{
									startDate2 = array6[0];
									flag3 = true;
								}
							}
							else
							{
								AsteroidCount++;
								text21 = array6[0];
								if (!flag2)
								{
									startDate = array6[0];
									flag2 = true;
								}
							}
							continue;
						}
						if (text22.Contains("<SolveFlags>"))
						{
							text4 = StripTags(text22);
							text5 = StripTags(streamReader.ReadLine());
							text6 = StripTags(streamReader.ReadLine());
							continue;
						}
						if (text22.Contains("<Shape"))
						{
							int num14 = 0;
							do
							{
								text22 = streamReader.ReadLine();
								if (text22.Contains("</Shape"))
								{
									break;
								}
								array[num14] = StripTags(text22);
								num14++;
							}
							while (!streamReader.EndOfStream);
							continue;
						}
						if (text22.Contains("<Satellite>"))
						{
							num4++;
							switch (num4)
							{
							case 1:
								text8 = StripTags(text22);
								break;
							case 2:
								text9 = StripTags(text22);
								break;
							}
							continue;
						}
						if (text22.Contains("<JDSO>"))
						{
							text19 = StripTags(text22);
						}
						double Correlation;
						double rmsStarDec_Total_mas;
						double rmsAlongTrack_Total_mas;
						if (text22.Contains("<Solution>"))
						{
							string[] array7 = StripTags(text22).Split(new char[1] { '|' });
							if (array7[2].Length > 0)
							{
								double.TryParse(array7[2], out var result3);
								if (result3 < 0.0 || result3 > 15.0)
								{
									array7[2] = "";
								}
							}
							if (array7[3].Length > 0)
							{
								double.TryParse(array7[1], out var result4);
								double.TryParse(array7[3], out var result5);
								if (result5 < 0.0 || result5 > 0.25 * result4)
								{
									array7[3] = "";
								}
							}
							string text24 = "";
							for (int m = 0; m < array7.Length - 1; m++)
							{
								text24 = ((m != 1) ? (text24 + array7[m].Trim() + "|") : (text24 + array7[m].Trim().Replace("-", ">") + "|"));
							}
							array2[num5] = text24;
							num5++;
						}
						else if (text22.Contains("<ReferenceTime"))
						{
							text10 = Merge_Y_M_D(StripTags(text22), 0);
							string[] array8 = StripTags(text22).Split(new char[1] { '|' });
							num = Utilities.JD_from_Date(int.Parse(array8[0]), int.Parse(array8[1]), double.Parse(array8[2]) + double.Parse(array8[3]) / 24.0);
							rmsTimeSecs = double.Parse(array8[4]);
							rmsAcrossPath_fromTime_mas = ((array8.Length <= 5) ? 0.0 : double.Parse(array8[5]));
						}
						else if (text22.Contains("<MainBody"))
						{
							string[] array9 = text5.Split(new char[1] { '|' });
							double num15 = double.Parse(array9[8]);
							double num16 = double.Parse(array9[9]);
							string text25 = StripTags(text22);
							string[] array10 = text25.Split(new char[1] { '|' });
							_ = array10[0] == "6";
							Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(num, num2, num3, use2006values_Not1976: false, num, double.Parse(array10[1]) + num15, double.Parse(array10[2]) + num16, out var EquatorialJ2000_X, out var EquatorialJ2000_Y, out var EquatorialJ2000_Z);
							array10[1].Substring(array10[1].IndexOf(".") + 1).Length.ToString();
							string text26 = SelectedFields(text25, 0, 2) + string.Format("{0,1:f3}|{1,1:f3}|{2,1:f3}|{3,1:f3}|{4,1:f3}|", num15, num16, EquatorialJ2000_X, EquatorialJ2000_Y, EquatorialJ2000_Z) + SelectedFields(text25, 3, array10.Length - 1);
							text26 = text26.Substring(0, text26.Length - 1);
							string text27 = StripTags(streamReader.ReadLine());
							text12 = Merge_Y_M_D_InDays(text27, 3);
							string[] array11 = text27.Split(new char[1] { '|' });
							Asteroid_Observations_Reports.CalculateUncertainties(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, rmsTimeSecs, rmsAcrossPath_fromTime_mas, motion_masSec, double.Parse(array11[7]), double.Parse(array11[9]), double.Parse(array11[10]) + 90.0, out var rmsRA_Total_mas, out var rmsDec_Total_mas, out Correlation, out var _, out rmsStarDec_Total_mas, out rmsAlongTrack_Total_mas);
							text11 = SelectedFields(text26, 0, 9) + string.Format("{0,1:f4}|{1,1:f4}|", rmsRA_Total_mas / 1000.0, rmsDec_Total_mas / 1000.0) + SelectedFields(text26, 12, array10.Length + 3);
						}
						else if (text22.Contains("<Secondary"))
						{
							num6++;
							switch (num6)
							{
							case 1:
							{
								string text31 = StripTags(text22);
								string[] array14 = text31.Split(new char[1] { '|' });
								Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(num, num2, num3, use2006values_Not1976: false, num, double.Parse(array14[1]), double.Parse(array14[2]), out var EquatorialJ2000_X3, out var EquatorialJ2000_Y3, out var EquatorialJ2000_Z3);
								array14[1].Substring(array14[1].IndexOf(".") + 1).Length.ToString();
								text13 = SelectedFields(text31, 0, 2) + string.Format("{0,1:f3}|{1,1:f3}|{2,1:f3}|", EquatorialJ2000_X3, EquatorialJ2000_Y3, EquatorialJ2000_Z3) + SelectedFields(text31, 3, array14.Length - 1);
								text13 = text13.Substring(0, text13.Length - 1);
								string text32 = StripTags(streamReader.ReadLine());
								text14 = Merge_Y_M_D_InDays(text32, 4);
								string[] array15 = text32.Split(new char[1] { '|' });
								Asteroid_Observations_Reports.CalculateUncertainties(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, rmsTimeSecs, rmsAcrossPath_fromTime_mas, motion_masSec, double.Parse(array15[8]), double.Parse(array15[10]), double.Parse(array15[11]) + 90.0, out var rmsRA_Total_mas3, out var rmsDec_Total_mas3, out rmsAlongTrack_Total_mas, out var _, out rmsStarDec_Total_mas, out Correlation);
								string text33 = array15[0];
								text13 = text33 + "|" + SelectedFields(text13, 1, 8) + string.Format("{0,1:f4}|{1,1:f4}|", rmsRA_Total_mas3 / 1000.0, rmsDec_Total_mas3 / 1000.0) + SelectedFields(text13, 11, array14.Length + 2);
								text13 = text13.Substring(0, text13.Length - 1);
								break;
							}
							case 2:
							{
								string text28 = StripTags(text22);
								string[] array12 = text28.Split(new char[1] { '|' });
								Utilities.FundamentalPlane_XY_to_GeoEquatorialXYZ(num, num2, num3, use2006values_Not1976: false, num, double.Parse(array12[1]), double.Parse(array12[2]), out var EquatorialJ2000_X2, out var EquatorialJ2000_Y2, out var EquatorialJ2000_Z2);
								array12[1].Substring(array12[1].IndexOf(".") + 1).Length.ToString();
								text15 = SelectedFields(text28, 0, 2) + string.Format("{0,1:f3}|{1,1:f3}|{2,1:f3}|", EquatorialJ2000_X2, EquatorialJ2000_Y2, EquatorialJ2000_Z2) + SelectedFields(text28, 3, array12.Length - 1);
								text15 = text15.Substring(0, text15.Length - 1);
								string text29 = StripTags(streamReader.ReadLine());
								text16 = Merge_Y_M_D_InDays(text29, 4);
								string[] array13 = text29.Split(new char[1] { '|' });
								Asteroid_Observations_Reports.CalculateUncertainties(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, rmsTimeSecs, rmsAcrossPath_fromTime_mas, motion_masSec, double.Parse(array13[8]), double.Parse(array13[10]), double.Parse(array13[11]) + 90.0, out var rmsRA_Total_mas2, out var rmsDec_Total_mas2, out Correlation, out var _, out rmsStarDec_Total_mas, out rmsAlongTrack_Total_mas);
								string text30 = array13[0];
								text15 = text30 + "|" + SelectedFields(text15, 1, 8) + string.Format("{0,1:f4}|{1,1:f4}|", rmsRA_Total_mas2 / 1000.0, rmsDec_Total_mas2 / 1000.0) + SelectedFields(text15, 11, array12.Length + 2);
								text15 = text15.Substring(0, text15.Length - 1);
								num6++;
								break;
							}
							}
						}
						else if (text22.Contains("<MPC"))
						{
							text7 = Merge_YMD(StripTags(text22), 0);
						}
						else if (text22.Contains("<Added>"))
						{
							text17 = Merge_Y_M_D(StripTags(text22), 0);
							if (text17 == "0-00-00|")
							{
								text17 = "1900-01-01|";
							}
						}
						else if (text22.Contains("<LastEdited>"))
						{
							text18 = Merge_Y_M_D(StripTags(text22), 0);
						}
						else if (text22.Contains("<Observer>"))
						{
							ObserverCount++;
							if (!flag)
							{
								streamWriter3.Write(AsteroidCount + "|");
								streamWriter3.Write(ReplaceXML_ReservedCharacters(d_m_sSplit(StripTags(streamReader.ReadLine()), 6, SecondField: true)).Replace("\"", ""));
								streamWriter3.Write(ReplaceXML_ReservedCharacters(Format_TimeShift_SNR(StripTags(streamReader.ReadLine()))).Replace("\"", ""));
								streamWriter3.Write(d_m_sSplit(StripTags(streamReader.ReadLine()), 0, SecondField: false));
								string text34 = d_m_sSplit(StripTags(streamReader.ReadLine()), 0, SecondField: false);
								streamWriter3.WriteLine(text34.Remove(text34.Length - 1));
								num7++;
							}
							else
							{
								streamWriter6.Write(PlanetCount + "|");
								streamWriter6.Write(ReplaceXML_ReservedCharacters(d_m_sSplit(StripTags(streamReader.ReadLine()), 6, SecondField: true)).Replace("\"", ""));
								streamWriter6.Write(ReplaceXML_ReservedCharacters(Format_TimeShift_SNR(StripTags(streamReader.ReadLine()))).Replace("\"", ""));
								streamWriter6.Write(d_m_sSplit(StripTags(streamReader.ReadLine()), 0, SecondField: false));
								string text35 = d_m_sSplit(StripTags(streamReader.ReadLine()), 0, SecondField: false);
								streamWriter6.WriteLine(text35.Remove(text35.Length - 1));
								num8++;
							}
						}
						else if (text22.Contains("<Prediction>"))
						{
							string[] array16 = StripTags(text22).Split(new char[1] { '|' });
							string text36 = d_m_sSplit(array16[0] + "|PREDICTION|||||" + array16[1] + "|" + array16[2], 6, SecondField: true);
							text36 += "|||||||||||";
							text36 = text36 + d_m_sSplit(array16[3], 0, SecondField: false) + "P|||||" + d_m_sSplit(array16[3], 0, SecondField: false) + "P||||";
							if (!flag)
							{
								streamWriter3.Write(AsteroidCount + "|");
								streamWriter3.WriteLine(text36);
								num7++;
							}
							else
							{
								streamWriter6.Write(PlanetCount + "|");
								streamWriter6.WriteLine(text36);
								num8++;
							}
						}
					}
					while (!streamReader.EndOfStream);
					text5 = SelectedFields(text5, 0, 7);
					if (!flag)
					{
						streamWriter.WriteLine(Utilities.HTML_DecodeString(AsteroidCount + "|" + text + text2 + text3 + text4 + text5 + text6 + array[0] + array[1] + array[2] + array[3] + array[4] + array[5] + text8 + text9 + array2[0] + array2[1] + array2[2] + array2[3] + text19 + text10 + text11 + text12 + text13 + text14 + text15 + text16 + text7 + text17 + text18.Remove(text18.Length - 1)));
						streamWriter2.WriteLine(Utilities.HTML_DecodeString(PlanetCount + "|" + SelectedFields(text, 0, 0) + SelectedFields(text3, 0, 1) + SelectedFields(text3, 10, 10) + SelectedFields(text5, 2, 6) + SelectedFields(text6, 0, 2) + array[0] + array[1] + array[2] + array[3] + array[4] + array[5].Remove(array[5].Length - 1)).Replace("\"", "''"));
					}
					else
					{
						streamWriter4.WriteLine(PlanetCount + "|" + text + text2 + text3 + text4 + text5 + text6 + array2[0] + array2[1] + array2[2] + array2[3] + text19 + text10 + text11 + text12 + text7 + text17 + text18.Remove(text18.Length - 1));
						string text37 = SelectedFields(text6, 0, 2);
						streamWriter5.WriteLine((PlanetCount + "|" + SelectedFields(text, 0, 0) + SelectedFields(text3, 0, 1) + SelectedFields(text3, 10, 10) + SelectedFields(text5, 2, 6) + text37.Remove(text37.Length - 1)).Replace("\"", "''"));
					}
					if (CreateTestFiles && AsteroidCount == 100)
					{
						break;
					}
				}
				while (!streamReader.EndOfStream);
			}
			((Control)messageForm.label).set_Text("2/6  Creating Astrometry file");
			Application.DoEvents();
			Asteroid_Observations_Reports.CreatePositionList(WithNames: false, Gaia_DR2_Updates: false, Asteroids: true, Planets: true, AsteroidSatellites: true, ListAll: true, 0, 2200, 3000000.0, 0, "", ObserverOnly: false, ExcludeStarsWithNoPM: false, IncludeExtraData: false);
			AsteroidPositions.SortField = 1;
			if (!CreateTestFiles)
			{
				Asteroid_Observations_Reports.PositionsList.Sort();
			}
			int num17 = 0;
			int num18 = 0;
			using (StreamWriter streamWriter7 = new StreamWriter(PDSyearPath + "\\" + Asteroid_xyzFile + ".psv"))
			{
				using StreamWriter streamWriter8 = new StreamWriter(PDSyearPath + "\\" + Planet_xyzFile + ".psv");
				streamWriter7.WriteLine(header6);
				streamWriter8.WriteLine(header6);
				for (int n = 0; n < Asteroid_Observations_Reports.PositionsList.Count && (!CreateTestFiles || n <= 100); n++)
				{
					string mPCLine_xyz = Asteroid_Observations_Reports.PositionsList[n].MPCLine_xyz;
					if (mPCLine_xyz.Length >= 10)
					{
						int startIndex = mPCLine_xyz.IndexOf('\n') + 1;
						if (mPCLine_xyz.Contains("143430|"))
						{
							streamWriter8.WriteLine(RemoveSpaces(mPCLine_xyz.Substring(startIndex).Replace("143430", "Pluto")));
							num18++;
						}
						else if (mPCLine_xyz.Contains("(143430)"))
						{
							streamWriter8.WriteLine(RemoveSpaces(mPCLine_xyz.Substring(startIndex)).Replace("(143430)", "Pluto"));
							num18++;
						}
						else if (Asteroid_Observations_Reports.PositionsList[n].IsParentPlanet == 0)
						{
							streamWriter7.WriteLine(RemoveSpaces(mPCLine_xyz));
							num17++;
						}
						else
						{
							streamWriter8.WriteLine(RemoveSpaces(mPCLine_xyz));
							num18++;
						}
					}
				}
			}
			((Control)messageForm.label).set_Text("3/6  Creating Diameters from shape models file");
			Application.DoEvents();
			Asteroid_Observations_Reports.ListDiametersFromShapeModelFits(ForPDS: true, PDSyearPath + "\\" + Diameters_File + ".psv", header7, out var DiameterCount);
			((Control)messageForm.label).set_Text("4/6  Creating Double star file");
			Application.DoEvents();
			Data_and_Plots.Historical_AllEvents.GetDoubles(All: true);
			AsteroidDoubleStars.SortField = 5;
			Asteroid_Observations_Reports.DoublesList.Sort();
			int num19 = 0;
			int num20 = 0;
			string text38 = "||||";
			using (StreamWriter streamWriter9 = new StreamWriter(PDSyearPath + "\\" + Doubles_File + ".psv"))
			{
				streamWriter9.WriteLine(header8);
				int num21;
				for (; num19 < Asteroid_Observations_Reports.DoublesList.Count; num19 += num21)
				{
					num21 = 1;
					try
					{
						if (Asteroid_Observations_Reports.DoublesList[num19 + 3].SolutionNumber == 4)
						{
							num21 = 4;
						}
						else if (Asteroid_Observations_Reports.DoublesList[num19 + 2].SolutionNumber == 3)
						{
							num21 = 3;
						}
						else if (Asteroid_Observations_Reports.DoublesList[num19 + 1].SolutionNumber == 2)
						{
							num21 = 2;
						}
					}
					catch
					{
					}
					string text39 = Asteroid_Observations_Reports.DoublesList[num19].StarID.Trim().Replace(" ", "|");
					string text40 = Asteroid_Observations_Reports.DoublesList[num19].Year + "-" + Asteroid_Observations_Reports.DoublesList[num19].Month.ToString().PadLeft(2, '0') + "-" + Asteroid_Observations_Reports.DoublesList[num19].Day.ToString().PadLeft(2, '0');
					string text41 = Asteroid_Observations_Reports.DoublesList[num19].WDS_id + "|" + Asteroid_Observations_Reports.DoublesList[num19].CompanionID.Trim() + "|" + text39 + "|" + text40 + "|" + string.Format("{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num19].Mv1) + "|" + string.Format("{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num19].Mv1);
					for (int num22 = 0; num22 < 4; num22++)
					{
						num20 = num19 + num22;
						if (num22 < num21)
						{
							text41 += string.Format("|{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num20].PA_deg);
							text41 = ((!(Asteroid_Observations_Reports.DoublesList[num20].Sdev_PA_Star_deg > 0.0)) ? (text41 + "|") : (text41 + string.Format("|{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num20].Sdev_PA_Star_deg)));
							text41 += string.Format("|{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num20].Sep_mas).Replace("-", ">");
							text41 = ((!((Asteroid_Observations_Reports.DoublesList[num20].Sdev_Sep_Star_mas > 0.0) & (Asteroid_Observations_Reports.DoublesList[num20].Sdev_Sep_Star_mas < 10.0))) ? (text41 + "|") : (text41 + string.Format("|{0,1:f1}", Asteroid_Observations_Reports.DoublesList[num20].Sdev_Sep_Star_mas)));
						}
						else
						{
							text41 += text38;
						}
					}
					text41 += string.Format("|{0,1:f7}", Asteroid_Observations_Reports.DoublesList[num19].RA2000 * (180.0 / Math.PI) / 15.0);
					text41 += string.Format("|{0,1:f7}", Asteroid_Observations_Reports.DoublesList[num19].Dec2000 * (180.0 / Math.PI));
					text41 = text41 + "|" + Asteroid_Observations_Reports.DoublesList[num19].AsteroidNumber.Trim() + "|" + Asteroid_Observations_Reports.DoublesList[num19].AsteroidID.Trim() + "|" + Asteroid_Observations_Reports.DoublesList[num19].JDSO_SubmitDate + "|" + Asteroid_Observations_Reports.DoublesList[num19].JDSO_Vol_Num_Pg;
					if (Asteroid_Observations_Reports.DoublesList[num19].Sep_mas < 1000.0)
					{
						streamWriter9.WriteLine(text41);
						num9++;
					}
				}
			}
			((Control)messageForm.label).set_Text("5/6  Creating Satellite file");
			Application.DoEvents();
			Data_and_Plots.Historical_AllEvents.GetEventBinaryData();
			BinaryAsteroid.SortField = 1;
			Asteroid_Observations_Reports.BinaryList.Sort();
			string[] array17 = new string[5] { "None", "Is it a satellite ?", "Approx offset", "Offset + size", "Offset + shape" };
			using (StreamWriter streamWriter10 = new StreamWriter(PDSyearPath + "\\" + Satellite_File + ".psv"))
			{
				num19 = 0;
				streamWriter10.WriteLine(header9);
				for (; num19 < Asteroid_Observations_Reports.BinaryList.Count; num19++)
				{
					string text42 = Asteroid_Observations_Reports.BinaryList[num19].Year + "-" + Asteroid_Observations_Reports.BinaryList[num19].Month.ToString().PadLeft(2, '0') + "-" + Asteroid_Observations_Reports.BinaryList[num19].Day.ToString().PadLeft(2, '0') + "|";
					string text43 = Asteroid_Observations_Reports.BinaryList[num19].AsteroidNumber.Trim() + "|" + Asteroid_Observations_Reports.BinaryList[num19].AsteroidID.Trim() + "|" + Asteroid_Observations_Reports.BinaryList[num19].SatelliteID.Trim() + "|";
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(text43 + text42);
					stringBuilder.Append(Asteroid_Observations_Reports.BinaryList[num19].CBET + "|");
					stringBuilder.Append(string.Format("{0,1:f1}|", Asteroid_Observations_Reports.BinaryList[num19].Sep));
					stringBuilder.Append(string.Format("{0,1:f1}|", Asteroid_Observations_Reports.BinaryList[num19].PA));
					stringBuilder.Append(string.Format("{0,1:f1}|", Asteroid_Observations_Reports.BinaryList[num19].MajorAxisCompanion));
					stringBuilder.Append(string.Format("{0,1:f1}|", Asteroid_Observations_Reports.BinaryList[num19].MinorAxisCompanion));
					stringBuilder.Append(string.Format("{0,1:f1}|", Asteroid_Observations_Reports.BinaryList[num19].PAMajorAxisCompanion));
					stringBuilder.Append(array17[Asteroid_Observations_Reports.BinaryList[num19].FitQualityCompanion]);
					streamWriter10.WriteLine(stringBuilder.ToString());
					num10++;
				}
			}
			((Control)messageForm.label).set_Text("6/6  Finalizing : Creating PDS label files");
			Application.DoEvents();
			string pDSauthors = Settings.Default.PDSauthors;
			string text44 = DateTime.Now.Year.ToString();
			string creationDate = text44 + "-" + DateTime.Now.Month.ToString().PadLeft(2, '0') + "-" + DateTime.Now.Day.ToString().PadLeft(2, '0');
			string description = "Occultations by asteroids - details of the event circumstances, and derived data";
			string description2 = "Occultations by planets and their satellites - details of the event circumstances, and derived data";
			string description3 = "Occultations by asteroids - summary of data derived from an occultation";
			string description4 = "Occultations by planets and their satellites - summary of data derived from an occultation";
			string description5 = "Occultations by asteroids - details of the observers and their observations";
			string description6 = "Occultations by planets and their satellites - details of the observers and their observations";
			string description7 = "Occultations by asteroids - derived astrometry";
			string description8 = "Occultations by planets and their satellites - derived astrometry";
			string description9 = "Diameters of asteroids from Shape model fitting";
			string description10 = "Double stars from occultations";
			string description11 = "Asteroidal satellites from occultations";
			string fileLength = new FileInfo(PDSyearPath + "\\" + AsteroidFile + ".psv").Length.ToString();
			string fileLength2 = new FileInfo(PDSyearPath + "\\" + AsteroidTimesFile + ".psv").Length.ToString();
			string fileLength3 = new FileInfo(PDSyearPath + "\\" + AsteroidSummaryFile + ".psv").Length.ToString();
			string fileLength4 = new FileInfo(PDSyearPath + "\\" + PlanetFile + ".psv").Length.ToString();
			string fileLength5 = new FileInfo(PDSyearPath + "\\" + PlanetTimesFile + ".psv").Length.ToString();
			string fileLength6 = new FileInfo(PDSyearPath + "\\" + PlanetSummaryFile + ".psv").Length.ToString();
			string fileLength7 = new FileInfo(PDSyearPath + "\\" + Asteroid_xyzFile + ".psv").Length.ToString();
			string fileLength8 = new FileInfo(PDSyearPath + "\\" + Planet_xyzFile + ".psv").Length.ToString();
			string fileLength9 = new FileInfo(PDSyearPath + "\\" + Diameters_File + ".psv").Length.ToString();
			string fileLength10 = new FileInfo(PDSyearPath + "\\" + Doubles_File + ".psv").Length.ToString();
			string fileLength11 = new FileInfo(PDSyearPath + "\\" + Satellite_File + ".psv").Length.ToString();
			using (StreamWriter streamWriter11 = new StreamWriter(PDSyearPath + "\\" + AsteroidFile + ".xml"))
			{
				streamWriter11.Write(XMLformatSpecification(ref L, header.Length + 2, AsteroidCount, "Asteroid occultations - event details", pDSauthors, text44, creationDate, startDate, text21, description, AsteroidFile, fileLength));
			}
			using (StreamWriter streamWriter12 = new StreamWriter(PDSyearPath + "\\" + AsteroidTimesFile + ".xml"))
			{
				streamWriter12.Write(XMLformatSpecification(ref L5, header5.Length + 2, num7, "Asteroid occultations - observed times", pDSauthors, text44, creationDate, startDate, text21, description5, AsteroidTimesFile, fileLength2));
			}
			using (StreamWriter streamWriter13 = new StreamWriter(PDSyearPath + "\\" + AsteroidSummaryFile + ".xml"))
			{
				streamWriter13.Write(XMLformatSpecification(ref L3, header3.Length + 2, AsteroidCount, "Asteroid occultations - summary of results", pDSauthors, text44, creationDate, startDate, text21, description3, AsteroidSummaryFile, fileLength3));
			}
			using (StreamWriter streamWriter14 = new StreamWriter(PDSyearPath + "\\" + PlanetFile + ".xml"))
			{
				streamWriter14.Write(XMLformatSpecification(ref L2, header2.Length + 2, PlanetCount, "Planet occultation - event details", pDSauthors, text44, creationDate, startDate2, endDate, description2, PlanetFile, fileLength4));
			}
			using (StreamWriter streamWriter15 = new StreamWriter(PDSyearPath + "\\" + PlanetTimesFile + ".xml"))
			{
				streamWriter15.Write(XMLformatSpecification(ref L5, header5.Length + 2, num8, "Planet and satellite occultations - observed times", pDSauthors, text44, creationDate, startDate2, endDate, description6, PlanetTimesFile, fileLength5));
			}
			using (StreamWriter streamWriter16 = new StreamWriter(PDSyearPath + "\\" + PlanetSummaryFile + ".xml"))
			{
				streamWriter16.Write(XMLformatSpecification(ref L4, header4.Length + 2, PlanetCount, "Planet and satellite occultations - summary of results", pDSauthors, text44, creationDate, startDate2, endDate, description4, PlanetSummaryFile, fileLength6));
			}
			using (StreamWriter streamWriter17 = new StreamWriter(PDSyearPath + "\\" + Asteroid_xyzFile + ".xml"))
			{
				streamWriter17.Write(XMLformatSpecification(ref L6, header6.Length + 2, num17, "Astrometry of Asteroids from occultations", pDSauthors, text44, creationDate, startDate, text21, description7, Asteroid_xyzFile, fileLength7));
			}
			using (StreamWriter streamWriter18 = new StreamWriter(PDSyearPath + "\\" + Planet_xyzFile + ".xml"))
			{
				streamWriter18.Write(XMLformatSpecification(ref L6, header6.Length + 2, num18, "Astrometry of Planets and their satellites from occultations", pDSauthors, text44, creationDate, startDate2, endDate, description8, Planet_xyzFile, fileLength8));
			}
			using (StreamWriter streamWriter19 = new StreamWriter(PDSyearPath + "\\" + Diameters_File + ".xml"))
			{
				streamWriter19.Write(XMLformatSpecification(ref L7, header7.Length + 2, DiameterCount, "Asteroid diameters from occultations fitted to shape models", pDSauthors, text44, creationDate, startDate, text21, description9, Diameters_File, fileLength9));
			}
			using (StreamWriter streamWriter20 = new StreamWriter(PDSyearPath + "\\" + Doubles_File + ".xml"))
			{
				streamWriter20.Write(XMLformatSpecification(ref L8, header8.Length + 2, num9, "Double stars discovered in asteroid occultations", pDSauthors, text44, creationDate, startDate, text21, description10, Doubles_File, fileLength10));
			}
			using (StreamWriter streamWriter21 = new StreamWriter(PDSyearPath + "\\" + Satellite_File + ".xml"))
			{
				streamWriter21.Write(XMLformatSpecification(ref L9, header9.Length + 2, num10, "Asteroidal satellites observed in asteroid occultations", pDSauthors, text44, creationDate, startDate, text21, description11, Satellite_File, fileLength11));
			}
			string[] array18 = text21.Split(new char[1] { '-' });
			EndCoverage = Utilities.Months[int.Parse(array18[1])] + " " + array18[0];
			string Y;
			using (StreamReader streamReader2 = new StreamReader(PDSHeaderFilePath + "PDS Abstract.txt"))
			{
				Y = streamReader2.ReadToEnd();
			}
			ReplaceTagsWithFileNames(ref Y);
			using (StreamWriter streamWriter22 = new StreamWriter(PDSyearPath + "\\PDS Abstract.txt"))
			{
				streamWriter22.Write(Y);
			}
			string Y2;
			using (StreamReader streamReader3 = new StreamReader(PDSHeaderFilePath + "PDS Known issues.txt"))
			{
				Y2 = streamReader3.ReadToEnd();
			}
			ReplaceTagsWithFileNames(ref Y2);
			using (StreamWriter streamWriter23 = new StreamWriter(PDSyearPath + "\\PDS Known issues.txt"))
			{
				streamWriter23.Write(Y2);
			}
			string Y3;
			using (StreamReader streamReader4 = new StreamReader(PDSHeaderFilePath + "PDS Package Description.txt"))
			{
				Y3 = streamReader4.ReadToEnd();
			}
			ReplaceTagsWithFileNames(ref Y3);
			using (StreamWriter streamWriter24 = new StreamWriter(PDSyearPath + "\\PDS Package Description.txt"))
			{
				streamWriter24.Write(Y3);
			}
			((Form)messageForm).Close();
		}

		internal static string StripTags(string Line)
		{
			int num = Line.IndexOf(">");
			Line = Line.Substring(num + 1);
			return Line[..Line.IndexOf("<")].Trim() + "|";
		}

		private static string SelectedFields(string Line, int FirstField, int LastField)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			for (int i = FirstField; i <= LastField; i++)
			{
				text = text + array[i].Trim() + "|";
			}
			return text;
		}

		private static string Merge_Y_M_D(string Line, int YearField)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			for (int i = 0; i < YearField; i++)
			{
				text = text + array[i] + "|";
			}
			text = text + array[YearField] + "-" + array[YearField + 1].PadLeft(2, '0') + "-" + array[YearField + 2].PadLeft(2, '0') + "|";
			for (int j = YearField + 3; j < array.Length - 1; j++)
			{
				text = text + array[j].Trim() + "|";
			}
			return text;
		}

		private static string Merge_Y_M_D_InDays(string Line, int YearField)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			for (int i = 0; i < YearField; i++)
			{
				text = text + array[i] + "|";
			}
			int num = array[YearField + 2].IndexOf('.');
			text = text + array[YearField] + "-" + array[YearField + 1].PadLeft(2, '0') + "-" + array[YearField + 2].Substring(0, num).PadLeft(2, '0') + "|" + array[YearField + 2].Substring(num) + "|";
			for (int j = YearField + 3; j < array.Length - 1; j++)
			{
				text = text + array[j].Trim() + "|";
			}
			return text;
		}

		private static string Merge_YMD(string Line, int DateField)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			for (int i = 0; i < DateField; i++)
			{
				text = text + array[i] + "|";
			}
			text = ((array[DateField].Length == 8) ? (text + array[DateField].Substring(0, 4) + "-" + array[DateField].Substring(4, 2).PadLeft(2, '0') + "-" + array[DateField].Substring(6, 2).PadLeft(2, '0') + "|") : (text + "|"));
			for (int j = DateField + 1; j < array.Length - 1; j++)
			{
				text = text + array[j].Trim() + "|";
			}
			return text;
		}

		private static string d_m_sSplit(string Line, int DMS_Field, bool SecondField)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			for (int i = 0; i < DMS_Field; i++)
			{
				text = text + array[i] + "|";
			}
			int num = DMS_Field;
			if (!SecondField)
			{
				text = ((array[num].Replace(".", "").Trim().Length != 0) ? (text + array[num].Trim().Replace("-  ", "-").Replace("- ", "-")
					.Replace("+  ", "+")
					.Replace("+ ", "+")
					.Replace("   ", "|")
					.Replace("  ", "|")
					.Replace(" ", "|") + "|") : (text + "|||"));
			}
			else
			{
				text = ((!array[num].Contains("-")) ? (text + "E|") : (text + "E|"));
				text = text + array[num].Trim().Replace("-  ", "").Replace("- ", "")
					.Replace("+  ", "")
					.Replace("+ ", "")
					.Replace("   ", "|")
					.Replace("  ", "|")
					.Replace(" ", "|") + "|";
			}
			if (SecondField)
			{
				num++;
				text = ((!array[num].Contains("-")) ? (text + "N|") : (text + "S|"));
				text = text + array[num].Trim().Replace("-  ", "").Replace("- ", "")
					.Replace("+  ", "")
					.Replace("+ ", "")
					.Replace("   ", "|")
					.Replace("  ", "|")
					.Replace(" ", "|") + "|";
			}
			num++;
			for (int j = num; j < array.Length - 1; j++)
			{
				text = text + array[j].Trim() + "|";
			}
			return text;
		}

		private static string Format_TimeShift_SNR(string Line)
		{
			string text = "";
			string[] array = Line.Split(new char[1] { '|' });
			string text2 = array[2];
			if (text2.Length > 0)
			{
				int num = text2.IndexOf(".");
				if (num < 0)
				{
					text2 += ".";
				}
				num = text2.IndexOf(".");
				array[2] = text2.PadRight(10).Substring(0, num + 2);
			}
			string text3 = array[3];
			if (text3.Length > 0)
			{
				int num2 = text3.IndexOf(".");
				if (num2 < 0)
				{
					text3 += ".";
				}
				num2 = text3.IndexOf(".");
				array[3] = text3.PadRight(10).Substring(0, num2 + 3);
			}
			for (int i = 0; i < array.Length - 1; i++)
			{
				text = text + array[i].Trim() + "|";
			}
			return text;
		}

		private static string ReplaceXML_ReservedCharacters(string Line)
		{
			return Line.Replace("&amp;", "&").Replace("&lt;", "<").Replace("&gt;", ">")
				.Replace("&quot;", "\"")
				.Replace("&apos;", "'");
		}

		private static void FillList(ref List<string[]> L, string Fname)
		{
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\Headers\\" + Fname + ".txt");
			do
			{
				string Line = streamReader.ReadLine();
				AddFormatFieldLocation(ref Line);
				string[] array = Line.Split(new char[1] { ':' });
				if (!((array[0] == "Label") | (array.Length != 5)))
				{
					string[] array2 = new string[5];
					for (int i = 0; i < 5; i++)
					{
						array2[i] = array[i];
					}
					L.Add(array2);
				}
			}
			while (!streamReader.EndOfStream);
		}

		internal static void AddFormatFieldLocation(ref string Line)
		{
			int num = 0;
			string text = Line;
			for (int i = 0; i < text.Length; i++)
			{
				if (text[i] == ':')
				{
					num++;
				}
			}
			if (num == 3)
			{
				int startIndex = Line.IndexOf(':', Line.IndexOf(':') + 1);
				Line = Line.Insert(startIndex, ":");
			}
		}

		private static string XMLformatSpecification(ref List<string[]> L, int HeaderLength, int RecordCount, string Title, string Authors, string Year, string CreationDate, string StartDate, string EndDate, string Description, string FileName, string FileLength)
		{
			string text = "";
			string text2 = "\r\n";
			string text3 = "  ";
			string text4 = "    ";
			string text5 = "      ";
			string text6 = "        ";
			string text7 = "          ";
			string text8 = "            ";
			text = text + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + text2;
			text = text + "<?xml-model href=\"https://pds.nasa.gov/pds4/pds/v1/PDS4_PDS_1K00.sch\" schematypens=\"http://purl.oclc.org/dsdl/schematron\"?>" + text2;
			text = text + "<Product_Observational xmlns=\"http://pds.nasa.gov/pds4/pds/v1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://pds.nasa.gov/pds4/pds/v1 https://pds.nasa.gov/pds4/pds/v1/PDS4_PDS_1K00.xsd\">" + text2;
			text = text + text3 + "<Identification_Area>" + text2;
			text = text + text4 + "<logical_identifier>urn:nasa:pds:smallbodiesoccultations:data:" + FileName.ToLower() + "</logical_identifier>" + text2;
			text = text + text4 + "<version_id>" + PDS_Version + "</version_id>" + text2;
			text = text + text4 + "<title>" + Title + "</title>" + text2;
			text = text + text4 + "<information_model_version>1.20.0.0</information_model_version>" + text2;
			text = text + text4 + "<product_class>Product_Observational</product_class>" + text2;
			text = text + text4 + "<Citation_Information>" + text2;
			text = text + text5 + "<author_list>" + Authors + "</author_list>" + text2;
			text = text + text5 + "<publication_year>" + Year + "</publication_year>" + text2;
			text = text + text5 + "<description>" + Description + "</description>" + text2;
			text = text + text4 + "</Citation_Information>" + text2;
			text = text + text3 + "</Identification_Area>" + text2;
			text = text + text3 + "<Observation_Area>" + text2;
			text = text + text4 + "<Time_Coordinates>" + text2;
			text = text + text5 + "<start_date_time>" + StartDate + "Z</start_date_time>" + text2;
			text = text + text5 + "<stop_date_time>" + EndDate + "Z</stop_date_time>" + text2;
			text = text + text4 + "</Time_Coordinates>" + text2;
			text = text + text4 + "<Primary_Result_Summary>" + text2;
			text = text + text5 + "<purpose>Science</purpose>" + text2;
			text = text + text5 + "<processing_level>Derived</processing_level>" + text2;
			text = text + text5 + "<Science_Facets>" + text2;
			text = text + text6 + "<discipline_name>Small Bodies</discipline_name>" + text2;
			text = text + text6 + "<facet1>Shape Model</facet1>" + text2;
			text = text + text5 + "</Science_Facets>" + text2;
			text = text + text5 + "<Science_Facets>" + text2;
			text = text + text6 + "<discipline_name>Ring-Moon Systems</discipline_name>" + text2;
			text = text + text6 + "<facet1>Satellite Astrometry</facet1>" + text2;
			text = text + text5 + "</Science_Facets>" + text2;
			text = text + text5 + "<Science_Facets>" + text2;
			text = text + text6 + "<discipline_name>Small Bodies</discipline_name>" + text2;
			text = text + text6 + "<facet1>Physical Properties</facet1>" + text2;
			text = text + text5 + "</Science_Facets>" + text2;
			text = text + text4 + "</Primary_Result_Summary>" + text2;
			text = text + text4 + "<Investigation_Area>" + text2;
			text = text + text5 + "<name>None</name>" + text2;
			text = text + text5 + "<type>Other Investigation</type>" + text2;
			text = text + text5 + "<Internal_Reference>" + text2;
			text = text + text6 + "<lid_reference>urn:nasa:pds:context:investigation:individual.none</lid_reference>" + text2;
			text = text + text6 + "<reference_type>data_to_investigation</reference_type>" + text2;
			text = text + text5 + "</Internal_Reference>" + text2;
			text = text + text4 + "</Investigation_Area>" + text2;
			text = text + text4 + "<Observing_System>" + text2;
			text = text + text5 + "<Observing_System_Component>" + text2;
			text = text + text6 + "<name>Various Ground-Based Telescopes</name>" + text2;
			text = text + text6 + "<type>Telescope</type>" + text2;
			text = text + text6 + "<description>This instrument host is used for those datasets that consist of surveys taken," + text2;
			text = text + text7 + "usually over long periods of time, at various ground-based observatories.The" + text2;
			text = text + text7 + "VARGBTEL host is used when it is not possible or practical to attempt to determine" + text2;
			text = text + text7 + "which specific observatory and/or telescope was used for individual results reported." + text2;
			text = text + text7 + "Some additional more specific information may be available in the published" + text2;
			text = text + text7 + "references cited in the corresponding data set description." + text2;
			text = text + text6 + "</description>" + text2;
			text = text + text5 + "</Observing_System_Component>" + text2;
			text = text + text5 + "<Observing_System_Component>" + text2;
			text = text + text6 + "<name>Various Ground-Based Detectors</name>" + text2;
			text = text + text6 + "<type>Instrument</type>" + text2;
			text = text + text6 + "<description>This instrument name is used when a data set consists of observations taken, usually" + text2;
			text = text + text7 + "over a long period of time, by various types of detectors attached to one or more" + text2;
			text = text + text7 + "ground-based telescopes.The VARGBDET identifier is used when it is not possible or" + text2;
			text = text + text7 + "not practical to identify even the generic type of detector associated with the" + text2;
			text = text + text7 + "individual observations comprising the data set.The published references listed in" + text2;
			text = text + text7 + "the corresponding data set description may provide more information." + text2;
			text = text + text6 + "</description>" + text2;
			text = text + text5 + "</Observing_System_Component>" + text2;
			text = text + text4 + "</Observing_System>" + text2;
			text = text + text4 + "<Target_Identification>" + text2;
			if (FileName.Contains("Asteroid"))
			{
				text = text + text5 + "<name>Multiple Asteroids</name>" + text2;
				text = text + text5 + "<type>Asteroid</type>" + text2;
			}
			else
			{
				text = text + text5 + "<name>Multiple Planets</name>" + text2;
				text = text + text5 + "<type>Planet</type>" + text2;
				text = text + text4 + "</Target_Identification>" + text2;
				text = text + text4 + "<Target_Identification>" + text2;
				text = text + text5 + "<name>Multiple Planet satellites</name>" + text2;
				text = text + text5 + "<type>Satellite</type>" + text2;
			}
			text = text + text4 + "</Target_Identification>" + text2;
			text = text + text3 + "</Observation_Area>" + text2;
			text = text + text3 + "<File_Area_Observational>" + text2;
			text = text + text4 + "<File>" + text2;
			text = text + text5 + "<file_name>" + FileName + ".psv</file_name>" + text2;
			text = text + text5 + "<local_identifier>" + FileName.ToLower() + "</local_identifier>" + text2;
			text = text + text5 + "<creation_date_time>" + CreationDate + "</creation_date_time>" + text2;
			text = text + text5 + "<file_size unit=\"byte\">" + FileLength + "</file_size>" + text2;
			text = text + text5 + "<records>" + RecordCount + "</records>" + text2;
			text = text + text4 + "</File>" + text2;
			text = text + text4 + "<Header>" + text2;
			text = text + text5 + "<offset unit=\"byte\">0</offset>" + text2;
			text = text + text5 + "<object_length unit=\"byte\">" + HeaderLength + "</object_length>" + text2;
			text = text + text5 + "<parsing_standard_id>UTF-8 Text</parsing_standard_id>" + text2;
			text = text + text4 + "</Header>" + text2;
			text = text + text4 + "<Table_Delimited>" + text2;
			text = text + text5 + "<offset unit=\"byte\">" + HeaderLength + "</offset>" + text2;
			text = text + text5 + "<parsing_standard_id>PDS DSV 1</parsing_standard_id>" + text2;
			text = text + text5 + "<records>" + RecordCount + "</records>" + text2;
			text = text + text5 + "<record_delimiter>Carriage-Return Line-Feed</record_delimiter>" + text2;
			text = text + text5 + "<field_delimiter>Vertical Bar</field_delimiter>" + text2;
			text = text + text5 + "<Record_Delimited>" + text2;
			text = text + text6 + "<fields>" + L.Count + "</fields>" + text2;
			text = text + text6 + "<groups>0</groups>" + text2;
			for (int i = 0; i < L.Count; i++)
			{
				text = text + text6 + "<Field_Delimited>" + text2;
				text = text + text7 + "<name>" + L[i][0] + "</name>" + text2;
				text = text + text7 + "<field_number>" + (i + 1) + "</field_number>" + text2;
				text = text + text7 + "<data_type>" + L[i][1] + "</data_type>" + text2;
				if (L[i][2].Length > 0)
				{
					text = text + text7 + "<field_format>" + L[i][2] + "</field_format>" + text2;
				}
				text = text + text7 + "<unit>" + L[i][3] + "</unit>" + text2;
				if (L[i][4].Contains("~"))
				{
					string[] array = L[i][4].Split(new char[1] { '~' });
					text = text + text7 + "<description>" + array[0] + text2;
					for (int j = 1; j < array.Length; j++)
					{
						text = text + text8 + array[j] + text2;
					}
					text = text + text7 + "</description>" + text2;
				}
				else
				{
					text = text + text7 + "<description>" + L[i][4] + "</description>" + text2;
				}
				text = text + text6 + "</Field_Delimited>" + text2;
			}
			text = text + text5 + "</Record_Delimited>" + text2;
			text = text + text4 + "</Table_Delimited>" + text2;
			text = text + text3 + "</File_Area_Observational>" + text2;
			return text + "</Product_Observational>" + text2;
		}

		private static string GetHeader(ref List<string[]> L)
		{
			string text = "";
			for (int i = 0; i < L.Count - 1; i++)
			{
				text = text + L[i][0].Trim() + "|";
			}
			return text + L[L.Count - 1][0];
		}

		private static string RemoveSpaces(string InLine)
		{
			int length;
			int length2;
			do
			{
				length = InLine.Length;
				InLine = InLine.Replace("        ", "");
				InLine = InLine.Replace("       ", "");
				InLine = InLine.Replace("      ", "");
				InLine = InLine.Replace("     ", "");
				InLine = InLine.Replace("    ", "");
				InLine = InLine.Replace("   ", "");
				InLine = InLine.Replace("  ", "");
				InLine = InLine.Replace("| ", "|");
				InLine = InLine.Replace(" (", "(");
				length2 = InLine.Length;
			}
			while (length > length2);
			InLine = InLine.Replace(" V", "V");
			InLine = InLine.Replace(" M", "M");
			InLine = InLine.Replace(" J", "J");
			InLine = InLine.Replace(" S", "S");
			InLine = InLine.Replace(" U", "U");
			InLine = InLine.Replace(" N", "N");
			InLine = InLine.Replace(" P", "P");
			return InLine;
		}

		private static void ReplaceTagsWithFileNames(ref string Y)
		{
			Y = Y.Replace(TagAsteroid, AsteroidFile + ".psv");
			Y = Y.Replace(TagAsteroidSummary, AsteroidSummaryFile + ".psv");
			Y = Y.Replace(TagAsteroidTimes, AsteroidTimesFile + ".psv");
			Y = Y.Replace(TagPlanet, PlanetFile + ".psv");
			Y = Y.Replace(TagPlanetSummary, PlanetSummaryFile + ".psv");
			Y = Y.Replace(TagPlanetTimes, PlanetTimesFile + ".psv");
			Y = Y.Replace(TagAsteroid_xyz, Asteroid_xyzFile + ".psv");
			Y = Y.Replace(TagPlanet_xyz, Planet_xyzFile + ".psv");
			Y = Y.Replace(TagDiameters, Diameters_File + ".psv");
			Y = Y.Replace(TagDoubles, Doubles_File + ".psv");
			Y = Y.Replace(TagSatellites, Satellite_File + ".psv");
			Y = Y.Replace(TagEventCount, EventCount.ToString());
			Y = Y.Replace(TagObserverCount, ObserverCount.ToString());
			Y = Y.Replace(TagLastDate, EndCoverage);
		}
	}
}
