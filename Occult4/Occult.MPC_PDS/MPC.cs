using System;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Properties;

namespace Occult.MPC_PDS
{
	internal class MPC
	{
		internal static string ADES_Version = "2022";

		internal static string MPC_Path = Utilities.AppPath + "\\Asteroid\\Results\\MPC Files";

		internal static DisplayData Display_Data;

		public static void CreateMPC_Report(bool Asteroids, bool Planets, bool AsteroidSatellites, bool IncludeAll, bool ByYear, int YearStart, int YearEnd, double LastJD, bool LatestYearOnly, int AsteroidNumber, string Observer, bool ObserverOnly, bool XYZ_Report, bool ExcludeStarsWithNoPM, bool IncludeExtraData)
		{
			//IL_01af: Unknown result type (might be due to invalid IL or missing references)
			//IL_0978: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = "";
			string text3 = "";
			new MessageForm();
			if (AsteroidNumber > 0)
			{
				text2 = "_for_(" + AsteroidNumber + ") " + Elements.MainAsteroids.GetAsteroidName_fromNumber(AsteroidNumber, out var _);
			}
			if (!Directory.Exists(MPC_Path))
			{
				Directory.CreateDirectory(MPC_Path);
			}
			if (YearStart < 2000)
			{
				YearStart = 1999;
			}
			if (YearEnd < YearStart)
			{
				YearEnd = YearStart;
			}
			if (YearEnd < 2000)
			{
				YearEnd = 1999;
			}
			if (LatestYearOnly)
			{
				YearStart = (YearEnd = DateTime.Now.Year);
				ByYear = true;
				IncludeAll = true;
			}
			if (!Asteroids)
			{
				YearStart = 1700;
				YearEnd = 2100;
			}
			if (!ByYear)
			{
				YearStart = 1700;
				YearEnd = 2100;
			}
			string text4 = "_xyz";
			if (!XYZ_Report)
			{
				text4 = "_RaDec";
			}
			if (Observer.Length > 0)
			{
				text4 = text4 + "_" + Observer;
			}
			if (ObserverOnly)
			{
				text4 += "_Only";
			}
			if (IncludeExtraData)
			{
				text4 += "_WithExtra";
			}
			text4 += ".psv";
			for (int i = YearStart; i <= YearEnd; i++)
			{
				if (ByYear)
				{
					text = "_for_" + i;
				}
				if (ByYear)
				{
					Asteroid_Observations_Reports.CreatePositionList(WithNames: true, Gaia_DR2_Updates: false, Asteroids, Planets, AsteroidSatellites, IncludeAll, i, i, LastJD, AsteroidNumber, Observer, ObserverOnly, ExcludeStarsWithNoPM, IncludeExtraData);
				}
				else
				{
					Asteroid_Observations_Reports.CreatePositionList(WithNames: true, Gaia_DR2_Updates: false, Asteroids, Planets, AsteroidSatellites, IncludeAll, YearStart, YearEnd, LastJD, AsteroidNumber, Observer, ObserverOnly, ExcludeStarsWithNoPM, IncludeExtraData);
				}
				AsteroidPositions.SortField = 1;
				AsteroidPositions.IncludeExtraDataInXYZ = IncludeExtraData;
				if ((Asteroid_Observations_Reports.PositionsList.Count < 1) | (Asteroid_Observations_Reports.ObserverList.Count < 1))
				{
					MessageBox.Show("No positions to report" + text, "No positions", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				Asteroid_Observations_Reports.PositionsList.Sort();
				Asteroid_Observations_Reports.ObserverList.Sort();
				for (int num = Asteroid_Observations_Reports.ObserverList.Count - 1; num > 0; num--)
				{
					if (Asteroid_Observations_Reports.ObserverList[num].ObserverID == Asteroid_Observations_Reports.ObserverList[num - 1].ObserverID)
					{
						Asteroid_Observations_Reports.ObserverList.RemoveAt(num);
					}
				}
				if (Asteroid_Observations_Reports.ObserverList[0].ObserverID.Trim() == "")
				{
					Asteroid_Observations_Reports.ObserverList.RemoveAt(0);
				}
				using (StreamWriter streamWriter = new StreamWriter(MPC_Path + "\\MPC report" + text + text2 + "_" + DateTime.Today.Year + DateTime.Today.Month.ToString().PadLeft(2, '0') + DateTime.Today.Day.ToString().PadLeft(2, '0') + text4))
				{
					streamWriter.WriteLine("# version=" + Settings.Default.ADES_version);
					streamWriter.WriteLine("# observatory");
					if (XYZ_Report)
					{
						streamWriter.WriteLine("! mpcCode 275");
					}
					else
					{
						streamWriter.WriteLine("! mpcCode 244");
					}
					if (!IncludeExtraData)
					{
						streamWriter.WriteLine("# submitter");
						streamWriter.WriteLine("! name " + Settings.Default.MPC_CON);
						streamWriter.WriteLine("# measurers");
						string[] array = Settings.Default.MPC_MEA.Split(new char[1] { ',' });
						for (int j = 0; j < array.Length; j++)
						{
							streamWriter.WriteLine("! name " + array[j].Trim());
						}
					}
					if (!IncludeExtraData)
					{
						streamWriter.WriteLine("# observers");
						for (int k = 0; k < Asteroid_Observations_Reports.ObserverList.Count; k++)
						{
							if (Asteroid_Observations_Reports.ObserverList[k].ObserverID.Contains("?"))
							{
								text3 = text3 + Asteroid_Observations_Reports.ObserverList[k].ObserverID + "  " + Asteroid_Observations_Reports.ObserverList[k].EventID + "\r\n";
							}
							else if (!Asteroid_Observations_Reports.ObserverList[k].ObserverID.Contains("Uni") && (Asteroid_Observations_Reports.ObserverList[k].ObserverID.Length <= 5 || !(Asteroid_Observations_Reports.ObserverList[k].ObserverID.Substring(3, 3) == "Obs")) && Asteroid_Observations_Reports.ObserverList[k].ObserverID.Length > 3 && Asteroid_Observations_Reports.ObserverList[k].ObserverID.Substring(1, 2) == ". ")
							{
								if (Asteroid_Observations_Reports.ObserverList[k].ObserverID.Length == 4)
								{
									text3 = text3 + Asteroid_Observations_Reports.ObserverList[k].ObserverID + "  " + Asteroid_Observations_Reports.ObserverList[k].EventID + "\r\n";
								}
								if (int.TryParse(Asteroid_Observations_Reports.ObserverList[k].ObserverID.Substring(Asteroid_Observations_Reports.ObserverList[k].ObserverID.Length - 1, 1), out var _))
								{
									text3 = text3 + Asteroid_Observations_Reports.ObserverList[k].ObserverID + "  " + Asteroid_Observations_Reports.ObserverList[k].EventID + "\r\n";
								}
								streamWriter.WriteLine("! name " + Asteroid_Observations_Reports.ObserverList[k].ObserverID.Replace("_", " "));
							}
						}
						if (text3.Length > 0)
						{
							try
							{
								((Control)Display_Data).Show();
							}
							catch
							{
								Display_Data = new DisplayData();
								((Control)Display_Data).Show();
							}
							((Control)Display_Data).set_Text("Anomalous observer names");
							((Control)Display_Data.txtBox).set_Text(text3);
						}
						streamWriter.WriteLine("# telescope");
						streamWriter.WriteLine("! design reflector");
						streamWriter.WriteLine("! aperture 0.2");
						streamWriter.WriteLine("! detector CCD");
					}
					if (XYZ_Report)
					{
						streamWriter.WriteLine("permID    |provID           |mode|stn|obsTime                |raStar       |decStar      |deltaRA |deltaDec|sys    |ctr|pos1     |pos2     |pos3     |rmsRA |rmsDec|rmsCorr|astCat  |shapeOcc|remarks                                            ");
						for (int l = 0; l < Asteroid_Observations_Reports.PositionsList.Count; l++)
						{
							string mPCLine_xyz = Asteroid_Observations_Reports.PositionsList[l].MPCLine_xyz;
							if (mPCLine_xyz.Length < 10)
							{
								continue;
							}
							int length = mPCLine_xyz.Length;
							int num2 = mPCLine_xyz.IndexOf('\n') + 1;
							if (num2 > 0)
							{
								length = num2 - 2;
							}
							if (Asteroids && Planets && AsteroidSatellites)
							{
								streamWriter.WriteLine(mPCLine_xyz);
								continue;
							}
							if (Asteroids)
							{
								if ((Asteroid_Observations_Reports.PositionsList[l].IsParentPlanet == 0) & !Asteroid_Observations_Reports.PositionsList[l].UnseenPrimary)
								{
									streamWriter.WriteLine(mPCLine_xyz.Substring(0, length));
								}
								else if ((Asteroid_Observations_Reports.PositionsList[l].IsParentPlanet == 2) & !Asteroid_Observations_Reports.PositionsList[l].UnseenPrimary)
								{
									streamWriter.WriteLine(mPCLine_xyz.Substring(0, length));
								}
							}
							if (AsteroidSatellites)
							{
								if (Asteroid_Observations_Reports.PositionsList[l].UnseenPrimary)
								{
									streamWriter.WriteLine(mPCLine_xyz);
								}
								else if (mPCLine_xyz.Substring(0, 8) == "    Mars")
								{
									streamWriter.WriteLine(mPCLine_xyz);
								}
								else if (num2 > 0)
								{
									streamWriter.WriteLine(mPCLine_xyz.Substring(num2));
								}
							}
							if (Planets)
							{
								if (Asteroid_Observations_Reports.PositionsList[l].IsParentPlanet == 1)
								{
									streamWriter.WriteLine(mPCLine_xyz);
								}
								else if (Asteroid_Observations_Reports.PositionsList[l].IsParentPlanet == 1)
								{
									streamWriter.WriteLine(mPCLine_xyz);
								}
							}
						}
					}
					else
					{
						streamWriter.WriteLine("permID    |provID           |mode|stn|obsTime                |raStar       |decStar      |deltaRA |deltaDec|rmsRA |rmsDec|rmsCorr|astCat  |shapeOcc|remarks                                            ");
						for (int m = 0; m < Asteroid_Observations_Reports.PositionsList.Count; m++)
						{
							string mPCLine = Asteroid_Observations_Reports.PositionsList[m].MPCLine;
							if (mPCLine.Length < 10)
							{
								continue;
							}
							if (Asteroids && Planets)
							{
								streamWriter.WriteLine(mPCLine);
							}
							else if (Asteroids)
							{
								if ((Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 0) | (Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 2))
								{
									streamWriter.WriteLine(mPCLine);
								}
								if (Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 3)
								{
									streamWriter.WriteLine(mPCLine.Substring(1 + mPCLine.Length / 2));
								}
							}
							else if (Planets)
							{
								if ((Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 1) | (Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 2))
								{
									streamWriter.WriteLine(mPCLine);
								}
								if (Asteroid_Observations_Reports.PositionsList[m].IsParentPlanet == 3)
								{
									streamWriter.WriteLine(mPCLine.Substring(1 + mPCLine.Length / 2));
								}
							}
						}
					}
				}
				if (!Asteroids || !ByYear)
				{
					break;
				}
			}
			MessageBox.Show("MPC report has been written to \r\n\r\n" + MPC_Path + "\r\n\r\nwith today's date", "MPC Report written", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}
	}
}
