using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	internal class AllEdits
	{
		public static List<EditLines> All_Edits = new List<EditLines>();

		public EditLines Edit_Lines;

		public static string Editor = "";

		public static string LastReadFile = "";

		internal static void ReadEditsFile(string EditFileName)
		{
			bool flag = true;
			All_Edits.Clear();
			EditLines editLines = new EditLines();
			if (new FileInfo(Utilities.AppPath + "\\Import_Export\\" + EditFileName).Length < 50)
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Import_Export\\" + EditFileName))
			{
				do
				{
					string text = streamReader.ReadLine();
					if ((text == "<AsteroidExport>") | (text == "</AsteroidExport>"))
					{
						continue;
					}
					if (text.Contains(Tags.TagStart[27]))
					{
						Editor = text.Replace(Tags.TagStart[27], "").Replace(Tags.TagEnd[27], "").Trim();
						continue;
					}
					if ((text.Trim() == "") | text.Contains(Tags.TagEnd[0]))
					{
						if (text.Contains(Tags.TagEnd[0]))
						{
							editLines.Edits.Add(text);
						}
						All_Edits.Add(editLines);
						flag = true;
						continue;
					}
					if (flag)
					{
						if (text.Contains(Tags.TagStart[0].Trim()))
						{
							editLines = new EditLines();
						}
						flag = false;
					}
					if (text.Contains(Tags.TagStart[1]))
					{
						AllEvents.Parse_EventDate(text, out var Year, out var Month, out var Day, out var MidT);
						editLines.EventJD = Utilities.JD_from_Date(Year, Month, (double)Day + MidT / 24.0);
						editLines.Date = Year + " " + Utilities.ShortMonths[Month] + " " + Day.ToString().PadLeft(2, '0');
					}
					if (text.Contains(Tags.TagStart[4]))
					{
						string[] array = text.Replace(Tags.TagStart[4], "").Replace(Tags.TagEnd[4], "").Trim()
							.Split(new char[1] { '|' });
						int.TryParse(array[0], out editLines.AstNum);
						if (array.Length > 1)
						{
							editLines.AsteroidName = array[1];
						}
					}
					editLines.Edits.Add(text);
				}
				while (!streamReader.EndOfStream);
				LastReadFile = EditFileName;
			}
			All_Edits.Sort();
		}

		internal static int EventInEditsFile(out string Comment)
		{
			bool flag = false;
			double jD_EventDate = EventDetails.JD_EventDate;
			int.TryParse(EventDetails.AsteroidNumber, out var result);
			Comment = "";
			int num = 0;
			int num2 = All_Edits.Count - 1;
			if (num2 < 0)
			{
				return -1;
			}
			int num3;
			do
			{
				num3 = (num + num2) / 2;
				if (Math.Abs(All_Edits[num3].EventJD - jD_EventDate) < 1.0)
				{
					flag = true;
					break;
				}
				if (All_Edits[num3].EventJD > jD_EventDate)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			if (flag)
			{
				num3 -= 20;
				if (num3 < 0)
				{
					num3 = 0;
				}
				do
				{
					if (Math.Abs(All_Edits[num3].EventJD - jD_EventDate) < 0.2 && All_Edits[num3].AstNum == result)
					{
						for (int i = 0; i < All_Edits[num3].Edits.Count; i++)
						{
							if (All_Edits[num3].Edits[i].Contains(Tags.TagStart[28]))
							{
								Comment = All_Edits[num3].Edits[i].Replace(Tags.TagStart[28], "").Replace(Tags.TagEnd[28], "").Trim()
									.Replace("^", "\r\n");
								break;
							}
						}
						return num3;
					}
					num3++;
				}
				while (num3 < All_Edits.Count && All_Edits[num3].EventJD - jD_EventDate < 1.0);
			}
			return -1;
		}

		internal static int EditEventInHistoricalFile(int EditEvent)
		{
			double eventJD = All_Edits[EditEvent].EventJD;
			double num = All_Edits[EditEvent].AstNum;
			bool flag = false;
			int num2 = 0;
			int num3 = Data_and_Plots.Historical_AllEvents.OccEvents.Count - 1;
			int num4;
			do
			{
				num4 = (num2 + num3) / 2;
				if (Data_and_Plots.Historical_AllEvents.OccEvents[num4].EventJD < eventJD - 1.0)
				{
					num2 = num4 + 1;
					continue;
				}
				if (Data_and_Plots.Historical_AllEvents.OccEvents[num4].EventJD > eventJD + 1.0)
				{
					num3 = num4 - 1;
					continue;
				}
				flag = true;
				break;
			}
			while (num2 <= num3);
			if (flag)
			{
				num4 -= 5;
				if (num4 < 0)
				{
					num4 = 0;
				}
				do
				{
					if (Math.Abs(Data_and_Plots.Historical_AllEvents.OccEvents[num4].EventJD - eventJD) < 0.2 && (double)Data_and_Plots.Historical_AllEvents.OccEvents[num4].AstNum == num)
					{
						return num4;
					}
					num4++;
				}
				while (num4 < Data_and_Plots.Historical_AllEvents.OccEvents.Count && Data_and_Plots.Historical_AllEvents.OccEvents[num4].EventJD - eventJD < 1.0);
			}
			return -1;
		}

		internal static void WriteEditsFile(string EditFileName)
		{
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Import_Export\\" + EditFileName);
			streamWriter.WriteLine("<AsteroidExport>");
			streamWriter.WriteLine("".PadRight(Tags.TagEditIndent[27]) + Tags.TagStart[27] + Editor + Tags.TagEnd[27]);
			for (int i = 0; i < All_Edits.Count; i++)
			{
				for (int j = 0; j < All_Edits[i].Edits.Count; j++)
				{
					streamWriter.WriteLine(All_Edits[i].Edits[j]);
				}
			}
			streamWriter.WriteLine("</AsteroidExport>");
		}

		internal static void AddEditedEvent(int Record)
		{
			EditLines editLines = new EditLines();
			editLines.Edits = XMLforAnEditedEvent();
			editLines.EventJD = Utilities.JD_from_Date(EventDetails.Year, EventDetails.Month, (double)EventDetails.Day + EventDetails.MidT_forMotions / 24.0);
			int.TryParse(EventDetails.AsteroidNumber, out editLines.AstNum);
			if (Record >= 0)
			{
				All_Edits[Record] = editLines;
			}
			else
			{
				All_Edits.Add(editLines);
			}
			All_Edits.Sort();
		}

		internal static List<string> XMLforAnEditedEvent()
		{
			List<string> list = new List<string>();
			list.Add("".PadRight(Tags.TagEditIndent[0]) + Tags.TagStart[0]);
			string item = "".PadLeft(Tags.TagEditIndent[1]) + Tags.TagStart[1] + EventDetails.Year + "|" + EventDetails.Month + "|" + EventDetails.Day + "|" + string.Format("{0,2:F1}", EventDetails.MidT_forMotions) + Tags.TagEnd[1];
			list.Add(item);
			string item2 = "".PadLeft(Tags.TagEditIndent[4]) + Tags.TagStart[4] + EventDetails.AsteroidNumber.Trim() + "|" + EventDetails.AsteroidID + Tags.TagEnd[4];
			list.Add(item2);
			string item3 = "".PadLeft(Tags.TagEditIndent[5]) + Tags.TagStart[5] + Convert.ToInt32(EventDetails.Solve_X) + "|" + Convert.ToInt32(EventDetails.Solve_Y) + "|" + Convert.ToInt32(EventDetails.Solve_Major) + "|" + Convert.ToInt32(EventDetails.Solve_Minor) + "|" + Convert.ToInt32(EventDetails.Solve_PA) + "|" + Convert.ToInt32(EventDetails.Solve_Circular) + "|" + Convert.ToInt32(EventDetails.Inc_Miss) + "|" + Convert.ToInt32(EventDetails.Solve_CompanionSep) + "|" + Convert.ToInt32(EventDetails.Solve_CompanionPA) + "|" + Tags.TagEnd[5];
			list.Add(item3);
			string item4 = "".PadLeft(Tags.TagEditIndent[6]) + Tags.TagStart[6] + string.Format("{0,2:F1}", EventDetails.X) + "|" + string.Format("{0,2:F1}", EventDetails.Y) + "|" + string.Format("{0,2:F1}", EventDetails.X_Dia) + "|" + string.Format("{0,2:F1}", EventDetails.Y_Dia) + "|" + string.Format("{0,2:F1}", EventDetails.PA_Ellipse) + "|" + EventDetails.Quality + "|" + Convert.ToInt32(EventDetails.UsedAssumedDiameter) + "|" + Convert.ToInt32(EventDetails.AstrometryShapeModelCentered) + Tags.TagEnd[6];
			list.Add(item4);
			string text = "".PadLeft(Tags.TagEditIndent[7]) + Tags.TagStart[7];
			text = ((!EventDetails.Sdev_X_Set) ? (text + "|") : (text + string.Format("{0,1:F1}|", EventDetails.Sdev_X)));
			text = ((!EventDetails.Sdev_Y_Set) ? (text + "|") : (text + string.Format("{0,1:F1}|", EventDetails.Sdev_Y)));
			text = ((!EventDetails.Sdev_Major_Set) ? (text + "|") : (text + string.Format("{0,1:F1}|", EventDetails.Sdev_Major)));
			text = ((!EventDetails.Sdev_Minor_Set) ? (text + "|") : (text + string.Format("{0,1:F1}|", EventDetails.Sdev_Minor)));
			if (EventDetails.Sdev_PA_Ellipse_Set)
			{
				text += string.Format("{0,1:F1}", EventDetails.Sdev_PA_Ellipse);
			}
			text += Tags.TagEnd[7];
			list.Add(text);
			if (EventDetails.ShapeData.Count > 0)
			{
				list.Add("".PadLeft(Tags.TagEditIndent[8]) + Tags.TagStart[8]);
				for (int i = 0; i < EventDetails.ShapeData.Count; i++)
				{
					list.Add(ShapeModelEditLine(i));
				}
				list.Add("".PadLeft(Tags.TagEditIndent[8]) + Tags.TagEnd[8]);
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				for (int j = 0; j < EventDetails.Satellites.Count; j++)
				{
					string item5 = "".PadLeft(Tags.TagEditIndent[10]) + Tags.TagStart[10] + SatelliteEditLine(j) + Tags.TagEnd[10];
					list.Add(item5);
				}
			}
			if (EventDetails.StarIsDouble & (EventDetails.Doubles.Count > 0))
			{
				list.Add("".PadLeft(Tags.TagEditIndent[16]) + Tags.TagStart[16]);
				list.Add("".PadLeft(Tags.TagIndent[40]) + Tags.TagStart[35] + EventDetails.JDSO_SubmitDate + "|" + EventDetails.JDSO_Vol_Num_Pg + Tags.TagEnd[35]);
				for (int k = 0; k < 4; k++)
				{
					if (EventDetails.Doubles[k].Sep_Companion > 0.0)
					{
						list.Add(DoubleStarEditLine(k));
					}
				}
				list.Add("".PadLeft(Tags.TagEditIndent[16]) + Tags.TagEnd[16]);
			}
			if (Data_and_Plots.Export_Comments != null && ((Control)Data_and_Plots.Export_Comments).get_Enabled())
			{
				string text2 = ((Control)Data_and_Plots.Export_Comments.txtComments).get_Text().Replace("\r\n", "^").Replace("<", ".lt.")
					.Replace(">", ".gt.");
				list.Add("".PadLeft(Tags.TagEditIndent[28]) + Tags.TagStart[28] + text2 + Tags.TagEnd[28]);
			}
			list.Add("".PadRight(Tags.TagEditIndent[26]) + Tags.TagStart[26] + DateTime.UtcNow.Year + "|" + DateTime.UtcNow.Month + "|" + DateTime.UtcNow.Day + Tags.TagEnd[26]);
			list.Add("".PadRight(Tags.TagEditIndent[0]) + Tags.TagEnd[0]);
			return list;
		}

		internal static string SatelliteEditLine(int k)
		{
			return EventDetails.Satellites[k].CompanionIAUname + "|" + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_dRA_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_dDec_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_d2RA_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_d2Dec_mas) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].SatelliteSeparation) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].SatellitePA_Apparent) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_Sep_Uncertainty) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].Sat_PA_Uncertainty) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].MajorAxisSatellite) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].MinorAxisSatellite) + string.Format("{0,1:F1}|", EventDetails.Satellites[k].PAAxisSatellite) + EventDetails.Satellites[k].SatelliteQuality;
		}

		internal static string ShapeModelEditLine(int i)
		{
			string text = "".PadLeft(Tags.TagEditIndent[9]) + Tags.TagStart[9] + EventDetails.ShapeData[i].Source + "|" + EventDetails.ShapeData[i].ID + "|" + string.Format("{0,5:f3}|", EventDetails.ShapeData[i].SurfaceVolumeRatio) + EventDetails.ShapeData[i].FitQuality + "|" + EventDetails.ShapeData[i].PhaseCorrn + "|";
			text = ((!(EventDetails.ShapeData[i].DiaMin > 30.0)) ? (text + string.Format("{0,2:f1}|", EventDetails.ShapeData[i].DiaMin) + string.Format("{0,2:f1}|", EventDetails.ShapeData[i].DiaMax)) : (text + string.Format("{0,2:f0}|", EventDetails.ShapeData[i].DiaMin) + string.Format("{0,2:f0}|", EventDetails.ShapeData[i].DiaMax)));
			return text + EventDetails.ShapeData[i].Version + Tags.TagEnd[9];
		}

		internal static string DoubleStarEditLine(int i)
		{
			return "".PadLeft(Tags.TagEditIndent[17]) + Tags.TagStart[17] + string.Format("{0,1:F1}|", EventDetails.Doubles[i].PA_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sep_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sdev_PA_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Sdev_Sep_Companion) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Centre_X) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Centre_Y) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Offset_X) + string.Format("{0,1:F1}|", EventDetails.Doubles[i].Offset_Y) + EventDetails.Doubles[i].SolutionID + Tags.TagEnd[17];
		}

		internal static void UpdateCurrentEventWithEdits(int EntryForImportEdit)
		{
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_0034: Unknown result type (might be due to invalid IL or missing references)
			//IL_003a: Invalid comparison between Unknown and I4
			bool flag = true;
			bool flag2 = true;
			bool flag3 = true;
			bool flag4 = true;
			bool flag5 = true;
			if (EntryForImportEdit < 0)
			{
				MessageBox.Show("There is no data to be imported", "No data to import", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			CompareSolutions compareSolutions = new CompareSolutions(EntryForImportEdit, ConfirmImport: true);
			if ((int)((Form)compareSolutions).ShowDialog() == 2)
			{
				return;
			}
			flag = compareSolutions.UpdateEllipse;
			flag2 = compareSolutions.UpdateFlags;
			flag3 = compareSolutions.UpdateShapes;
			flag4 = compareSolutions.UpdateDoubles;
			flag5 = compareSolutions.UpdateDoubles;
			int SolutionCount = 0;
			int satelliteCount = 0;
			if (flag4)
			{
				for (int i = 0; i < 4; i++)
				{
					DoubleData doubleData = new DoubleData();
					doubleData.Companion_Set = false;
					double num2 = (doubleData.Offset_Y = 0.0);
					double num4 = (doubleData.Offset_X = num2);
					double num6 = (doubleData.Centre_Y = num4);
					double num8 = (doubleData.Centre_X = num6);
					double num11 = (doubleData.Sep_Companion = (doubleData.PA_Companion = num8));
					EventDetails.Doubles[i] = doubleData;
				}
				EventDetails.StarIsDouble = false;
			}
			if (flag3)
			{
				EventDetails.ShapeData.Clear();
			}
			if (flag5)
			{
				DoubleData doubleData2 = new DoubleData();
				doubleData2.Companion_Set = false;
				double num2 = (doubleData2.Offset_Y = 0.0);
				double num4 = (doubleData2.Offset_X = num2);
				double num6 = (doubleData2.Centre_Y = num4);
				double num8 = (doubleData2.Centre_X = num6);
				double num11 = (doubleData2.Sep_Companion = (doubleData2.PA_Companion = num8));
				EventDetails.Doubles[4] = doubleData2;
			}
			Data_and_Plots.PlotForm.PreventUpdatingChanges = true;
			for (int j = 0; j < All_Edits[EntryForImportEdit].Edits.Count; j++)
			{
				string text = All_Edits[EntryForImportEdit].Edits[j];
				if (flag)
				{
					if (text.Contains(Tags.TagStart[6]))
					{
						Data_and_Plots.Historical_AllEvents.Parse_EllipticFit(text);
					}
					if (text.Contains(Tags.TagStart[7]))
					{
						Data_and_Plots.Historical_AllEvents.Parse_EllipticUncertainty(text);
					}
				}
				if (flag2)
				{
					if (text.Contains(Tags.TagStart[5]))
					{
						Data_and_Plots.Historical_AllEvents.Parse_SolveFlags(text);
					}
					Data_and_Plots.PlotForm.chkX.set_Checked(EventDetails.Solve_X);
					Data_and_Plots.PlotForm.chkY.set_Checked(EventDetails.Solve_Y);
					Data_and_Plots.PlotForm.chkA.set_Checked(EventDetails.Solve_Major);
					Data_and_Plots.PlotForm.chkB.set_Checked(EventDetails.Solve_Minor);
					Data_and_Plots.PlotForm.chkPA.set_Checked(EventDetails.Solve_PA);
					Data_and_Plots.PlotForm.chkCircle.set_Checked(EventDetails.Solve_Circular);
					Data_and_Plots.PlotForm.chkMiss.set_Checked(EventDetails.Inc_Miss);
					Data_and_Plots.PlotForm.chkCompanionSep.set_Checked(EventDetails.Solve_CompanionSep);
					Data_and_Plots.PlotForm.chkCompanionPA.set_Checked(EventDetails.Solve_CompanionPA);
				}
				if (flag3 && text.Contains(Tags.TagStart[9]))
				{
					Data_and_Plots.Historical_AllEvents.Parse_ShapeModelFit(text);
				}
				if (flag4 && text.Contains(Tags.TagStart[17]))
				{
					Data_and_Plots.Historical_AllEvents.Parse_DoubleStarSolution(text, ref SolutionCount);
				}
				if (flag5 && text.Contains(Tags.TagStart[10]))
				{
					Data_and_Plots.Historical_AllEvents.Parse_Satellite(text, satelliteCount);
				}
				if (text.Contains(Tags.TagStart[28]))
				{
					text.Trim().Replace(Tags.TagStart[28], "").Replace(Tags.TagEnd[28], "");
				}
			}
			if (flag)
			{
				Data_and_Plots.PlotForm.updnX.set_Value((decimal)EventDetails.X);
				Data_and_Plots.PlotForm.updnY.set_Value((decimal)EventDetails.Y);
				Data_and_Plots.PlotForm.updnA.set_Value((decimal)EventDetails.X_Dia);
				Data_and_Plots.PlotForm.updnB.set_Value((decimal)EventDetails.Y_Dia);
				Data_and_Plots.PlotForm.updnPA.set_Value((decimal)EventDetails.PA_Ellipse);
				((ListControl)Data_and_Plots.PlotForm.cmbQuality).set_SelectedIndex(EventDetails.Quality_To_cmbQuality);
				Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(EventDetails.UsedAssumedDiameter);
				CheckBox chkShapeModelCentered = Data_and_Plots.PlotForm.chkShapeModelCentered;
				bool astrometryShapeModelCentered;
				((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(astrometryShapeModelCentered = EventDetails.AstrometryShapeModelCentered);
				chkShapeModelCentered.set_Checked(astrometryShapeModelCentered);
			}
			Data_and_Plots.PlotForm.PreventUpdatingChanges = false;
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}
	}
}
