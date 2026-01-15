using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class DiameterFromChords : Form
	{
		internal static List<HistoricalIndexData> HistoricalIndex = new List<HistoricalIndexData>();

		private FileStream AD;

		private BinaryReader ADread;

		private IContainer components;

		private ListBox lstAsteroids;

		private ListBox lstChords;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withChordsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private RadioButton optName;

		private RadioButton optNumber;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private Label label1;

		public DiameterFromChords()
		{
			InitializeComponent();
		}

		private void DiameterFromChords_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			CreateAsteroidList();
		}

		private void CreateAsteroidList()
		{
			string text = "";
			string text2 = "";
			string text3 = "*";
			string text4 = "  ";
			int num = 0;
			int num2 = 0;
			HistoricalIndex.Clear();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				HistoricalIndexData historicalIndexData = new HistoricalIndexData();
				historicalIndexData.ReadIndexLine(Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine + i.ToString().PadLeft(5));
				num2++;
				HistoricalIndex.Add(historicalIndexData);
			}
			HistoricalIndexData.SortField = 0;
			if (optNumber.get_Checked())
			{
				HistoricalIndexData.SortField = 1;
			}
			HistoricalIndex.Sort();
			OpenDiameterFileForReading();
			lstAsteroids.get_Items().Clear();
			lstAsteroids.get_Items().Add((object)" Number  Name          #   Dia");
			for (int j = 0; j < HistoricalIndex.Count; j++)
			{
				if (HistoricalIndex[j].AsteroidID != text)
				{
					if (j > 0)
					{
						lstAsteroids.get_Items().Add((object)(text2.PadLeft(7) + "  " + text + num.ToString().PadLeft(2) + text3 + text4));
						text4 = "";
					}
					text = HistoricalIndex[j].AsteroidID;
					text2 = HistoricalIndex[j].AsteroidNumber;
					text3 = HasMeasuredDiameter(HistoricalIndex[j].AsteroidNum);
					if (HistoricalIndex[j].HasShapeModel)
					{
						text4 = "  sm";
					}
					num = 1;
				}
				else
				{
					num++;
				}
			}
			lstAsteroids.get_Items().Add((object)(text2.PadLeft(7) + "  " + text + num.ToString().PadLeft(2) + text3));
			CloseDiameterFileForReading();
		}

		private void OpenDiameterFileForReading()
		{
			AD = new FileStream(Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin", FileMode.Open, FileAccess.Read);
			ADread = new BinaryReader(AD);
		}

		private void CloseDiameterFileForReading()
		{
			AD.Close();
		}

		private string HasMeasuredDiameter(int AsteroidNumber)
		{
			double num = 0.0;
			string text = " ";
			if (AsteroidNumber < 550000 && AsteroidNumber > 0)
			{
				AD.Seek(2 * (AsteroidNumber - 1), SeekOrigin.Begin);
				num = (double)ADread.ReadInt16() / 10.0;
				if (num < 0.0)
				{
					text = "•";
				}
			}
			if (num == 0.0)
			{
				return "? ".PadLeft(7);
			}
			return string.Format("{0,7:f1}", Math.Abs(num)) + text;
		}

		private void lstAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(9, 13);
			string text = lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(0, 7).Trim();
			double num = 0.0;
			double[] array = new double[1000];
			int num2 = 0;
			lstChords.get_Items().Clear();
			lstChords.get_Items().Add((object)"Observed chord lengths");
			lstChords.get_Items().Add((object)"");
			lstChords.get_Items().Add((object)"Length      # Observer");
			if (Data_and_Plots.Observations_Editor == null)
			{
				Data_and_Plots.ShowEditor();
			}
			((Form)Data_and_Plots.Observations_Editor).set_WindowState((FormWindowState)1);
			if (Data_and_Plots.PlotForm == null)
			{
				Data_and_Plots.ShowPlotForm();
			}
			bool @checked = Data_and_Plots.PlotForm.mnuAlign.get_Checked();
			Data_and_Plots.PlotForm.mnuAlign.set_Checked(false);
			Asteroid_Observations_Reports.ShapesList.Clear();
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				string eventID = Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: false, Kepler2: false);
				if (!(eventID.Substring(0, 7).Trim() == text))
				{
					continue;
				}
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i);
				Data_and_Plots.FirstTimePlot = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
				lstChords.get_Items().Add((object)"");
				lstChords.get_Items().Add((object)eventID.TrimStart(Array.Empty<char>()));
				int num3 = 0;
				for (int j = 0; j < EventDetails.Observers.Count; j++)
				{
					double num4 = Data_and_Plots.X[j, 0] - Data_and_Plots.X[j, 1];
					double num5 = Data_and_Plots.Y[j, 0] - Data_and_Plots.Y[j, 1];
					double num6 = Math.Sqrt(num4 * num4 + num5 * num5);
					if ((num6 > 0.05 && num6 < 8000.0) & !EventDetails.Observers[j].Observer1.ToUpper().Contains("PREDICT"))
					{
						array[num2] = num6;
						num += num6;
						num2++;
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.AppendFormat("{0,6:F1} km", num6);
						stringBuilder.AppendFormat(" {0,3:F0} ", EventDetails.Observers[j].SeqNumber);
						stringBuilder.Append(EventDetails.Observers[j].ObserversAll);
						lstChords.get_Items().Add((object)stringBuilder.ToString());
						num3++;
					}
				}
				bool flag = false;
				for (int k = 0; k < EventDetails.ShapeData.Count; k++)
				{
					if (((EventDetails.ShapeData[k].FitQuality > 2) & (EventDetails.ShapeData[k].FitQuality < 7)) && !((EventDetails.ShapeData[k].DiaMin == 0.0) | (EventDetails.ShapeData[k].DiaMax == 0.0)))
					{
						if (!flag)
						{
							lstChords.get_Items().Add((object)"");
							lstChords.get_Items().Add((object)"    Shape model diameters from this event");
							flag = true;
						}
						StringBuilder stringBuilder2 = new StringBuilder();
						stringBuilder2.Append(EventDetails.ShapeData[k].Source.PadLeft(9) + EventDetails.ShapeData[k].ID.PadLeft(6) + ":");
						if (EventDetails.ShapeData[k].FitQuality == 3)
						{
							stringBuilder2.AppendFormat("  > {0,1:F1} km", EventDetails.ShapeData[k].DiaMin);
						}
						else
						{
							stringBuilder2.AppendFormat("  {0,1:F1} - {1,1:F1} km", EventDetails.ShapeData[k].DiaMin, EventDetails.ShapeData[k].DiaMax);
						}
						lstChords.get_Items().Add((object)stringBuilder2.ToString());
						ShapeModels shapeModels = new ShapeModels();
						shapeModels.Source = EventDetails.ShapeData[k].Source;
						shapeModels.ID = EventDetails.ShapeData[k].ID;
						shapeModels.FitQuality = EventDetails.ShapeData[k].FitQuality;
						shapeModels.VolumeDia_Min = EventDetails.ShapeData[k].DiaMin;
						shapeModels.VolumeDia_Max = EventDetails.ShapeData[k].DiaMax;
						Asteroid_Observations_Reports.ShapesList.Add(shapeModels);
					}
				}
				if (num3 == 0)
				{
					lstChords.get_Items().Add((object)"No valid chords from this event");
				}
			}
			ShapeModels.SortField = 1;
			Asteroid_Observations_Reports.ShapesList.Sort();
			List<double> list = new List<double>();
			int num7 = 0;
			string text2 = "";
			string text3 = "";
			bool flag2 = false;
			string text4 = "Shape model diameters from all events";
			double Mean;
			double SDev;
			for (int l = 0; l < Asteroid_Observations_Reports.ShapesList.Count; l++)
			{
				if (l == 0)
				{
					text2 = "    " + Asteroid_Observations_Reports.ShapesList[l].Source.PadRight(6) + Asteroid_Observations_Reports.ShapesList[l].ID.Trim().PadRight(11);
					num7 = 0;
				}
				else if (Asteroid_Observations_Reports.ShapesList[l].ID != Asteroid_Observations_Reports.ShapesList[l - 1].ID)
				{
					if (list.Count > 0)
					{
						if (!flag2)
						{
							flag2 = true;
							lstChords.get_Items().Add((object)"".PadRight(40, '_'));
							lstChords.get_Items().Add((object)text4);
						}
						Utilities.Mean_Sdev(list, out Mean, out SDev);
						text3 = ((!(Mean > 10.0)) ? "1" : "0");
						lstChords.get_Items().Add((object)(text2 + string.Format("{0,1:f" + text3 + "} ± {1,1:f" + text3 + "} km", Mean, SDev)));
					}
					list = new List<double>();
					text2 = "    " + Asteroid_Observations_Reports.ShapesList[l].Source.PadRight(6) + Asteroid_Observations_Reports.ShapesList[l].ID.Trim().PadRight(11);
					num7 = 0;
				}
				if (Asteroid_Observations_Reports.ShapesList[l].FitQuality == 4)
				{
					list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Max);
					list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Min);
					num7++;
				}
				else if (Asteroid_Observations_Reports.ShapesList[l].FitQuality == 5)
				{
					for (int m = 0; m < 3; m++)
					{
						list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Max);
						list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Min);
						num7++;
					}
				}
				else if (Asteroid_Observations_Reports.ShapesList[l].FitQuality == 6)
				{
					for (int n = 0; n < 9; n++)
					{
						list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Max);
						list.Add(Asteroid_Observations_Reports.ShapesList[l].VolumeDia_Min);
						num7++;
					}
				}
			}
			if (list.Count > 0)
			{
				Utilities.Mean_Sdev(list, out Mean, out SDev);
				if (!flag2)
				{
					lstChords.get_Items().Add((object)"".PadRight(40, '_'));
					lstChords.get_Items().Add((object)text4);
				}
				text3 = ((!(Mean > 10.0)) ? "1" : "0");
				lstChords.get_Items().Add((object)(text2 + string.Format("{0,1:f" + text3 + "} ± {1,1:f" + text3 + "} km", Mean, SDev)));
			}
			Data_and_Plots.PlotForm.mnuAlign.set_Checked(@checked);
			if (num2 > 2)
			{
				double num8 = 0.7 * num / (double)num2;
				double num9 = 0.0;
				int num10 = 0;
				for (int num11 = 0; num11 < num2; num11++)
				{
					if (array[num11] >= num8)
					{
						num9 += array[num11];
						num10++;
					}
				}
				lstChords.get_Items().Add((object)"".PadRight(40, '_'));
				lstChords.get_Items().Add((object)string.Format("Approximate asteroid diameter : {0,1:f0}km", 1.27 * num9 / (double)num10));
				lstChords.get_Items().Add((object)"(Derived using John Broughton's basic method)");
			}
			if (int.TryParse(text, out var result))
			{
				Utilities.Display_IR_AsteroidDiameter(result, ShowInForm: false, out var Diameters);
				string[] array2 = Diameters.Replace('\r', ' ').Split(new char[1] { '\n' });
				lstChords.get_Items().Add((object)"".PadRight(40, '_'));
				lstChords.get_Items().Add((object)"");
				for (int num12 = 0; num12 < array2.Length; num12++)
				{
					lstChords.get_Items().Add((object)(array2[num12] ?? ""));
				}
			}
			((Form)Data_and_Plots.PlotForm).Close();
		}

		internal string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < lstChords.get_Items().get_Count(); i++)
			{
				stringBuilder.Append(lstChords.get_Items().get_Item(i).ToString() + "\r\n");
			}
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(CollectEvents(), "ChordLengths (" + EventDetails.AsteroidNumber + ") " + EventDetails.FormattedDate, Utilities.AppPath + "\\Asteroid");
		}

		private void DiameterFromChords_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(769);
			ListBox obj = lstAsteroids;
			int height;
			((Control)lstChords).set_Height(height = ((Control)this).get_Height() - 92);
			((Control)obj).set_Height(height);
		}

		private void optName_CheckedChanged(object sender, EventArgs e)
		{
			CreateAsteroidList();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Chord lengths");
		}

		private void optNumber_CheckedChanged(object sender, EventArgs e)
		{
			CreateAsteroidList();
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "Observed chord lengths\r\n\r\nLength      # Observer\r\n\r\n";
			foreach (object selectedItem in lstChords.get_SelectedItems())
			{
				text = text + selectedItem.ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			lstAsteroids = new ListBox();
			lstChords = new ListBox();
			menuStrip1 = new MenuStrip();
			withChordsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			optName = new RadioButton();
			optNumber = new RadioButton();
			label1 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstAsteroids).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstAsteroids).set_FormattingEnabled(true);
			lstAsteroids.set_ItemHeight(14);
			((Control)lstAsteroids).set_Location(new Point(9, 47));
			((Control)lstAsteroids).set_Name("lstAsteroids");
			lstAsteroids.set_ScrollAlwaysVisible(true);
			((Control)lstAsteroids).set_Size(new Size(278, 550));
			((Control)lstAsteroids).set_TabIndex(0);
			lstAsteroids.add_SelectedIndexChanged((EventHandler)lstAsteroids_SelectedIndexChanged);
			((Control)lstChords).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstChords).set_FormattingEnabled(true);
			lstChords.set_ItemHeight(14);
			((Control)lstChords).set_Location(new Point(320, 47));
			((Control)lstChords).set_Name("lstChords");
			lstChords.set_ScrollAlwaysVisible(true);
			lstChords.set_SelectionMode((SelectionMode)3);
			((Control)lstChords).set_Size(new Size(435, 550));
			((Control)lstChords).set_TabIndex(1);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withChordsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(767, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withChordsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withChordsToolStripMenuItem).set_Name("withChordsToolStripMenuItem");
			((ToolStripItem)withChordsToolStripMenuItem).set_Size(new Size(101, 20));
			((ToolStripItem)withChordsToolStripMenuItem).set_Text("with Chords...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(149, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy ALL");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(149, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy Selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(149, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(149, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)optName).set_AutoSize(true);
			((Control)optName).set_Location(new Point(380, 3));
			((Control)optName).set_Name("optName");
			((Control)optName).set_Size(new Size(87, 17));
			((Control)optName).set_TabIndex(3);
			((Control)optName).set_Text("sort by Name");
			((ButtonBase)optName).set_UseVisualStyleBackColor(true);
			optName.add_CheckedChanged((EventHandler)optName_CheckedChanged);
			((Control)optNumber).set_AutoSize(true);
			optNumber.set_Checked(true);
			((Control)optNumber).set_Location(new Point(278, 3));
			((Control)optNumber).set_Name("optNumber");
			((Control)optNumber).set_Size(new Size(96, 17));
			((Control)optNumber).set_TabIndex(4);
			optNumber.set_TabStop(true);
			((Control)optNumber).set_Text("sort by Number");
			((ButtonBase)optNumber).set_UseVisualStyleBackColor(true);
			optNumber.add_CheckedChanged((EventHandler)optNumber_CheckedChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(12, 31));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(217, 14));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text(" Number  Name          #   Dia");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(767, 603));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)optNumber);
			((Control)this).get_Controls().Add((Control)(object)optName);
			((Control)this).get_Controls().Add((Control)(object)lstChords);
			((Control)this).get_Controls().Add((Control)(object)lstAsteroids);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DiameterFromChords");
			((Control)this).set_Text("Observed chord lengths");
			((Form)this).add_Load((EventHandler)DiameterFromChords_Load);
			((Control)this).add_Resize((EventHandler)DiameterFromChords_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
