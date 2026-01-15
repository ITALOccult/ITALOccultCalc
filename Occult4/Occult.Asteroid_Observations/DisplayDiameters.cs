using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class DisplayDiameters : Form
	{
		private double ModelDiameter;

		private string HeaderLine1 = "      # Name                    Date   SM IR-dia|  major    minor      PA     Dia    %diff|  Q crd|   Model 1       |   Model 2       |   Model 3       |   Model 4       |   Model 5       |   Model 6       |   Model 7           Model 8";

		private string HeaderLine2 = "";

		private bool ListDiameters;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withDiametersToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byQualityToolStripMenuItem;

		private ToolStripMenuItem byAsteroidToolStripMenuItem;

		private ToolStripMenuItem byMajorAxisToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstDiameters;

		private ToolStripMenuItem byAsteroidNameToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNumberShapeModelToolStripMenuItem;

		private CheckBox chkLimitToQ34;

		private Label lblHeader;

		private CheckBox chkIncludespacesHeaders;

		private ToolStripMenuItem iRSateComprisonsToolStripMenuItem;

		private ToolStripMenuItem noneToolStripMenuItem;

		private ToolStripMenuItem allTogetherToolStripMenuItem;

		private ToolStripMenuItem saveAscsvToolStripMenuItem;

		private CheckBox chkNoCircles;

		private CheckBox chk4Chords;

		private ToolStripMenuItem ellipseVsAllSatsToolStripMenuItem;

		private CheckBox chkShapeNum;

		private ComboBox cmbQcodes;

		private ComboBox cmbShapeCodes;

		private Label label1;

		private Label label2;

		private Label label3;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem displayShapeModelDiametersToolStripMenuItem;

		private Panel panelGeneral;

		private Label lblModelFits;

		private Label label5;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		public DisplayDiameters()
		{
			InitializeComponent();
			lstDiameters.set_HorizontalExtent(1500);
			ComboBox obj = cmbQcodes;
			int selectedIndex;
			((ListControl)cmbShapeCodes).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj).set_SelectedIndex(selectedIndex);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: true));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false), "Diameters from occultation observations", Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents(bool NoPlusMinus, bool SelectedLinesOnly)
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (SelectedLinesOnly)
			{
				for (int i = 0; i < lstDiameters.get_SelectedItems().get_Count(); i++)
				{
					string value = lstDiameters.get_SelectedItems().get_Item(i).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value);
				}
			}
			else
			{
				for (int j = 0; j < lstDiameters.get_Items().get_Count(); j++)
				{
					string value2 = lstDiameters.get_Items().get_Item(j).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value2);
				}
			}
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 0;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void byQualityToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 1;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void byAsteroidToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 2;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void byAsteroidNumberShapeModelToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 5;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void byAsteroidNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 3;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void byMajorAxisToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidDiameters.SortField = 4;
			Asteroid_Observations_Reports.DiameterList.Sort();
			Display();
		}

		private void chkLimitToQ34_CheckedChanged(object sender, EventArgs e)
		{
			Display();
		}

		private void chkIncludespacesHeaders_CheckedChanged(object sender, EventArgs e)
		{
			Display();
		}

		private void chk4Chords_CheckedChanged(object sender, EventArgs e)
		{
			Display();
		}

		private void chkNoCircles_CheckedChanged(object sender, EventArgs e)
		{
			Display();
		}

		private void chkShapeNum_CheckedChanged(object sender, EventArgs e)
		{
			Display();
		}

		public void Display()
		{
			lstDiameters.get_Items().Clear();
			if (!ListDiameters)
			{
				lstDiameters.get_Items().Add((object)"           *****   Diameters derived from asteroidal occultations   *****");
				lstDiameters.get_Items().Add((object)"");
				lstDiameters.get_Items().Add((object)("".PadRight(48) + "|   Ellipse fits  ------->                |Quality|  Volume-equivalent diameters from fits to Shape models   ------->"));
			}
			int num = 0;
			int num2 = 1;
			if (chkShapeNum.get_Checked())
			{
				num2 = 2;
			}
			bool flag = false;
			for (int i = 0; i < Asteroid_Observations_Reports.DiameterList.Count; i++)
			{
				int LineCount = 0;
				if (chkIncludespacesHeaders.get_Checked() & !ListDiameters)
				{
					if (num % 25 == 0 && !flag)
					{
						if (i > 0)
						{
							lstDiameters.get_Items().Add((object)"".PadRight(235, '_'));
						}
						lstDiameters.get_Items().Add((object)HeaderLine1);
						lstDiameters.get_Items().Add((object)"");
						flag = true;
					}
				}
				else if (i == 0)
				{
					if (!ListDiameters)
					{
						lstDiameters.get_Items().Add((object)HeaderLine1);
					}
					else
					{
						lstDiameters.get_Items().Add((object)HeaderLine2);
					}
				}
				if ((chkLimitToQ34.get_Checked() && Asteroid_Observations_Reports.DiameterList[i].Quality < 3) || (chkNoCircles.get_Checked() && Asteroid_Observations_Reports.DiameterList[i].Solve_Circular) || (chk4Chords.get_Checked() && Asteroid_Observations_Reports.DiameterList[i].NumChords < 3))
				{
					continue;
				}
				if (!ListDiameters)
				{
					lstDiameters.get_Items().Add((object)Asteroid_Observations_Reports.DiameterList[i].ToString());
				}
				else
				{
					if (GetModelComparsionWithShapeModels_All(ref i, out LineCount, out var ModelCount, out var Line) && ModelCount >= num2)
					{
						lstDiameters.get_Items().Add((object)Line);
					}
					i += LineCount - 1;
				}
				if (chkIncludespacesHeaders.get_Checked() & !ListDiameters)
				{
					if (num % 5 == 4)
					{
						lstDiameters.get_Items().Add((object)"");
					}
					num++;
					flag = false;
				}
			}
		}

		private void DisplayDiameters_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 100)
			{
				((Control)lstDiameters).set_Height(((Control)this).get_Height() - 113);
			}
			((Control)lstDiameters).set_Width(((Control)this).get_Width() - 26);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid Diameters - Display");
		}

		private void DisplayDiameters_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)lblHeader).set_Text(HeaderLine1);
		}

		private void noneToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListDiameters = false;
			((ToolStripItem)saveToolStripMenuItem).set_Visible(true);
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Visible(false);
			Display();
		}

		private void saveAscsvToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			//IL_0066: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			try
			{
				string value = "Descriptor AsteroidNumber,AsteroidName,EllipseDiameter,+-,EllipseCount,ModelFitDiameter,+-,ModelCount";
				string value2 = "Descriptor AsteroidNumberAll,AsteroidNameAll,EllipseDiameterAll,+-,EllipseCountAll,ModelFitDiameterAll,+-,ModelCountAll,DiameterAll,+-";
				string value3 = "Descriptor AsteroidNumberNeo,AsteroidNameNeo,EllipseDiameterNeo,+-,EllipseCountNeo,ModelFitDiameterNeo,+-,ModelCountNeo,DiameteNeo,+-";
				string value4 = "Descriptor AsteroidNumberAcuA,AsteroidNameAcuA,EllipseDiameterAcuA,+-,EllipseCountAcuA,ModelFitDiameterAcuA,+-,ModelCountAcuA,DiameterAcuA,+-";
				string value5 = "Descriptor AsteroidNumberIras,AsteroidNameIras,EllipseDiameterIras,+-,EllipseCountModel,ModelFitDiameterIras,+-,ModelCountIRAS,DiameterIRAS,+-";
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Generated Files");
				((FileDialog)val).set_Filter(".csv|*.csv|Text files|*.txt|All files|*.*");
				((FileDialog)val).set_DefaultExt(".csv");
				((FileDialog)val).set_Title("CSV file to save");
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				string text = Path.GetDirectoryName(((FileDialog)val).get_FileName()) + "\\" + Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName());
				using StreamWriter streamWriter = new StreamWriter(text + "_Ellipse.csv");
				streamWriter.WriteLine(value);
				using StreamWriter streamWriter2 = new StreamWriter(text + "_AllSats.csv");
				streamWriter2.WriteLine(value2);
				using StreamWriter streamWriter3 = new StreamWriter(text + "_NEOWISE.csv");
				streamWriter3.WriteLine(value3);
				using StreamWriter streamWriter4 = new StreamWriter(text + "_AcuA.csv");
				streamWriter4.WriteLine(value4);
				using StreamWriter streamWriter5 = new StreamWriter(text + "_IRAS.csv");
				streamWriter5.WriteLine(value5);
				for (int i = 1; i < lstDiameters.get_Items().get_Count(); i++)
				{
					string text2 = lstDiameters.get_Items().get_Item(i).ToString();
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(text2.Substring(0, 6).Trim() + ",");
					stringBuilder.Append(text2.Substring(7, 13).Trim() + ",");
					double.TryParse(text2.Substring(21, 7), out var result);
					double.TryParse(text2.Substring(30, 5), out var result2);
					stringBuilder.Append(string.Format("{0,1:f1},{1,1:f1},", result, result2));
					stringBuilder.Append(text2.Substring(35, 3).Trim() + ",");
					double.TryParse(text2.Substring(38, 8), out result);
					double.TryParse(text2.Substring(48, 5), out result2);
					stringBuilder.Append(string.Format("{0,1:f1},{1,1:f1},", result, result2));
					stringBuilder.Append(text2.Substring(53, 3).Trim() + ",");
					StringBuilder stringBuilder2 = new StringBuilder();
					StringBuilder stringBuilder3 = new StringBuilder();
					StringBuilder stringBuilder4 = new StringBuilder();
					StringBuilder stringBuilder5 = new StringBuilder();
					bool flag = false;
					bool flag2 = false;
					bool flag3 = false;
					bool flag4 = false;
					string text3 = text2.Substring(56, 8).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "";
					}
					else
					{
						flag = true;
					}
					stringBuilder2.Append(text3 + ",");
					text3 = text2.Substring(66, 5).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "0";
					}
					stringBuilder2.Append(text3);
					text3 = text2.Substring(71, 8).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "";
					}
					else
					{
						flag2 = true;
					}
					stringBuilder3.Append(text3 + ",");
					text3 = text2.Substring(81, 5).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "0";
					}
					stringBuilder3.Append(text3);
					text3 = text2.Substring(86, 8).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "";
					}
					else
					{
						flag3 = true;
					}
					stringBuilder4.Append(text3 + ",");
					text3 = text2.Substring(96, 5).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "0";
					}
					stringBuilder4.Append(text3);
					text3 = text2.Substring(101, 8).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "";
					}
					else
					{
						flag4 = true;
					}
					stringBuilder5.Append(text3 + ",");
					text3 = text2.Substring(111, 5).Trim();
					if (double.Parse(text3) == 0.0)
					{
						text3 = "0";
					}
					stringBuilder5.Append(text3);
					streamWriter.WriteLine(stringBuilder.ToString());
					if (flag)
					{
						streamWriter2.WriteLine(stringBuilder.ToString() + stringBuilder2.ToString());
					}
					if (flag2)
					{
						streamWriter3.WriteLine(stringBuilder.ToString() + stringBuilder3.ToString());
					}
					if (flag3)
					{
						streamWriter4.WriteLine(stringBuilder.ToString() + stringBuilder4.ToString());
					}
					if (flag4)
					{
						streamWriter5.WriteLine(stringBuilder.ToString() + stringBuilder5.ToString());
					}
				}
			}
			finally
			{
				((IDisposable)val)?.Dispose();
			}
		}

		private bool GetModelComparsionWithShapeModels_All(ref int k, out int LineCount, out int ModelCount, out string Line)
		{
			AsteroidDiameters.SortField = 2;
			Asteroid_Observations_Reports.DiameterList.Sort();
			double num = 0.0;
			ModelCount = 0;
			Line = "";
			LineCount = 0;
			List<double> list = new List<double>();
			List<double> list2 = new List<double>();
			int num2 = (int)Asteroid_Observations_Reports.DiameterList[k].AsteroidNum;
			string asteroidID = Asteroid_Observations_Reports.DiameterList[k].AsteroidID;
			Utilities.Get_Individual_AsteroidDiameter(num2, out var AllSats_Dia, out var AllSats_Uncert, out var NEOWISE_Diameter, out var NEOWISE_Uncert, out var Akari_Dia, out var Akari_Uncert, out var IRAS_Dia, out var IRAS_Uncert);
			ModelDiameter = 0.0;
			ModelCount = 0;
			num = 0.0;
			if (k + LineCount < Asteroid_Observations_Reports.DiameterList.Count - 1)
			{
				while ((int)Asteroid_Observations_Reports.DiameterList[k + LineCount].AsteroidNum == num2)
				{
					num += Asteroid_Observations_Reports.DiameterList[k + LineCount].EllipseDiameterUncert * Asteroid_Observations_Reports.DiameterList[k + LineCount].EllipseDiameterUncert;
					list.Add(Asteroid_Observations_Reports.DiameterList[k + LineCount].EllipseDiameter);
					for (int i = 0; i < Asteroid_Observations_Reports.DiameterList[k + LineCount].ShapeModelSource.Count; i++)
					{
						if (Asteroid_Observations_Reports.DiameterList[k + LineCount].ShapeModelSource[i] == "D" && "56".Contains(Asteroid_Observations_Reports.DiameterList[k + LineCount].ShapeModelFit[i]))
						{
							ModelDiameter += Asteroid_Observations_Reports.DiameterList[k + LineCount].ShapeModelDiameter[i];
							ModelCount++;
							list2.Add(Asteroid_Observations_Reports.DiameterList[k + LineCount].ShapeModelDiameter[i]);
						}
					}
					LineCount++;
					if (k + LineCount >= Asteroid_Observations_Reports.DiameterList.Count)
					{
						break;
					}
				}
				if (list2.Count > 0)
				{
					Utilities.Mean_Sdev(list, out var Mean, out var SDev);
					if (SDev == 0.0)
					{
						SDev = 5.0;
					}
					Utilities.Mean_Sdev(list2, out var Mean2, out var SDev2);
					if (SDev2 == 0.0)
					{
						SDev2 = 5.0;
					}
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,6:f0} ", num2);
					stringBuilder.Append(asteroidID.PadRight(13));
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", Mean, SDev);
					stringBuilder.AppendFormat("{0,3:f0}", LineCount);
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", Mean2, SDev2);
					stringBuilder.AppendFormat("{0,3:f0}", ModelCount);
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", AllSats_Dia, AllSats_Uncert);
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", NEOWISE_Diameter, NEOWISE_Uncert);
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", Akari_Dia, Akari_Uncert);
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", IRAS_Dia, IRAS_Uncert);
					Line = stringBuilder.ToString();
					return true;
				}
				return false;
			}
			return false;
		}

		private void allTogetherToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)lblHeader).set_Text(HeaderLine1);
			((Control)panelGeneral).set_Visible(true);
			((Control)lblModelFits).set_Visible(false);
			displayShapeModelDiametersToolStripMenuItem.set_Checked(false);
			ListDiameters = true;
			((ToolStripItem)saveToolStripMenuItem).set_Visible(false);
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Visible(true);
			((Control)lblHeader).set_Text("     # Name             Ellipse Fit  #    Model Fit    #  All Satellites    NEOWISE        AcuA           IRAS ");
			Display();
		}

		private void ellipseVsAllSatsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)lblHeader).set_Text(HeaderLine1);
			((Control)panelGeneral).set_Visible(true);
			((Control)lblModelFits).set_Visible(false);
			displayShapeModelDiametersToolStripMenuItem.set_Checked(false);
			lstDiameters.get_Items().Clear();
			int satellite = 0;
			if (Asteroid_Observations_Reports.EllipseDiameterList.Count == 0)
			{
				ReadEllipseDiameters(satellite);
			}
			ListEvents();
		}

		private void ListEvents()
		{
			string text = "Number Name             Ellipse Dia  #    NEOWISE Dia     AcuA dia       IRAS dia";
			lstDiameters.get_Items().Clear();
			((Control)lblHeader).set_Text(text);
			lstDiameters.get_Items().Add((object)text);
			for (int i = 0; i < Asteroid_Observations_Reports.EllipseDiameterList.Count; i++)
			{
				if (GetEllipseComparisonWithSatellites(ref i, out var _, out var Line))
				{
					lstDiameters.get_Items().Add((object)Line);
				}
			}
		}

		internal bool GetEllipseComparisonWithSatellites(ref int k, out int LineCount, out string Line)
		{
			Asteroid_Observations_Reports.EllipseDiameterList.Sort();
			Line = "";
			LineCount = 0;
			List<double> list = new List<double>();
			new List<double>();
			int asteroidNumber = Asteroid_Observations_Reports.EllipseDiameterList[k].AsteroidNumber;
			string asteroidName = Asteroid_Observations_Reports.EllipseDiameterList[k].AsteroidName;
			Utilities.Get_Individual_AsteroidDiameter(asteroidNumber, out var _, out var _, out var NEOWISE_Diameter, out var NEOWISE_Uncert, out var Akari_Dia, out var Akari_Uncert, out var IRAS_Dia, out var IRAS_Uncert);
			if (k + LineCount < Asteroid_Observations_Reports.EllipseDiameterList.Count - 1)
			{
				while (Asteroid_Observations_Reports.EllipseDiameterList[k + LineCount].AsteroidNumber == asteroidNumber)
				{
					if (!Asteroid_Observations_Reports.EllipseDiameterList[k + LineCount].Solve_Circular | !Asteroid_Observations_Reports.EllipseDiameterList[k + LineCount].SolveAssumed)
					{
						list.Add(Asteroid_Observations_Reports.EllipseDiameterList[k + LineCount].EllipseDiameter);
						if (k + LineCount >= Asteroid_Observations_Reports.EllipseDiameterList.Count)
						{
							continue;
						}
					}
					LineCount++;
				}
				k += LineCount - 1;
				if (LineCount > 1)
				{
					Utilities.Mean_Sdev(list, out var Mean, out var SDev);
					if (SDev == 0.0)
					{
						SDev = 5.0;
					}
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,6:f0} ", asteroidNumber);
					stringBuilder.Append(asteroidName.PadRight(13));
					stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", Mean, SDev);
					stringBuilder.AppendFormat("{0,3:f0}", LineCount);
					if (NEOWISE_Diameter > 0.0)
					{
						stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", NEOWISE_Diameter, NEOWISE_Uncert);
					}
					else
					{
						stringBuilder.Append("".PadRight(15));
					}
					if (Akari_Dia > 0.0)
					{
						stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", Akari_Dia, Akari_Uncert);
					}
					else
					{
						stringBuilder.Append("".PadRight(15));
					}
					if (IRAS_Dia > 0.0)
					{
						stringBuilder.AppendFormat("{0,8:f1} ±{1,5:f1}", IRAS_Dia, IRAS_Uncert);
					}
					else
					{
						stringBuilder.Append("".PadRight(15));
					}
					Line = stringBuilder.ToString();
					return true;
				}
				return false;
			}
			return false;
		}

		private void ReadEllipseDiameters(int Satellite)
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetEllipseDiameter();
			Asteroid_Observations_Reports.EllipseDiameterList.Sort();
		}

		private void lstDiameters_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_0010: Invalid comparison between Unknown and I4
			if (e.get_Control() && (int)e.get_KeyCode() == 67)
			{
				Clipboard.SetText(string.Join(Environment.NewLine, ((IEnumerable)lstDiameters.get_SelectedItems()).OfType<string>()));
			}
		}

		private void displayShapeModelDiametersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			displayShapeModelDiametersToolStripMenuItem.set_Checked(!displayShapeModelDiametersToolStripMenuItem.get_Checked());
			if (displayShapeModelDiametersToolStripMenuItem.get_Checked())
			{
				((Control)panelGeneral).set_Visible(false);
				((Control)lblModelFits).set_Visible(true);
				Application.DoEvents();
				Asteroid_Observations_Reports.ListDiametersFromShapeModelFits(ForPDS: false, Utilities.AppPath + "/Generated Files/ShapeModelDiameters.txt", "", out var _);
				lstDiameters.get_Items().Clear();
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "/Generated Files/ShapeModelDiameters.txt"))
				{
					do
					{
						lstDiameters.get_Items().Add((object)streamReader.ReadLine());
					}
					while (!streamReader.EndOfStream);
				}
				((Control)lblHeader).set_Text(lstDiameters.get_Items().get_Item(0).ToString());
			}
			else
			{
				((Control)lblHeader).set_Text(HeaderLine1);
				((Control)panelGeneral).set_Visible(true);
				((Control)lblModelFits).set_Visible(false);
				Display();
			}
		}

		private void lstDiameters_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152 || displayShapeModelDiametersToolStripMenuItem.get_Checked())
			{
				return;
			}
			try
			{
				string text = lstDiameters.get_SelectedItem().ToString();
				if (text.Trim().Length < 10)
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				if (text.Substring(0, 8).Contains("#") | text.Substring(0, 8).Contains("_") | (((ListControl)lstDiameters).get_SelectedIndex() == 0))
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				string asteroidNumber = text.Substring(0, 7);
				string asteroidName = text.Substring(8, 16).Trim();
				int.TryParse(text.Substring(27, 4), out var result);
				int month = 1;
				string text2 = text.Substring(32, 3);
				for (int i = 1; i < 13; i++)
				{
					if (Utilities.ShortMonths[i] == text2)
					{
						month = i;
					}
				}
				int.TryParse(text.Substring(36, 2), out var result2);
				Data_and_Plots.ShowEditor();
				Data_and_Plots.Observations_Editor.DisplayEvent_ByDate_Number(result, month, result2, asteroidNumber, asteroidName);
			}
			catch
			{
			}
		}

		private void ErrorMessage(int left, int top)
		{
			TimedMessageDisplay timedMessageDisplay = new TimedMessageDisplay();
			((Control)timedMessageDisplay).set_Text("Wrong line selected");
			((Control)timedMessageDisplay.txtMessage).set_Text("You must select the line that identifies the event");
			((Control)timedMessageDisplay).Show();
			((Control)timedMessageDisplay).set_Left(left + 10);
			((Control)timedMessageDisplay).set_Top(top + 10);
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_0afe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b08: Expected O, but got Unknown
			//IL_0b15: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b1f: Expected O, but got Unknown
			//IL_144e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1458: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(DisplayDiameters));
			menuStrip1 = new MenuStrip();
			withDiametersToolStripMenuItem = new ToolStripMenuItem();
			displayShapeModelDiametersToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAscsvToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byQualityToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNumberShapeModelToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNameToolStripMenuItem = new ToolStripMenuItem();
			byMajorAxisToolStripMenuItem = new ToolStripMenuItem();
			iRSateComprisonsToolStripMenuItem = new ToolStripMenuItem();
			allTogetherToolStripMenuItem = new ToolStripMenuItem();
			ellipseVsAllSatsToolStripMenuItem = new ToolStripMenuItem();
			noneToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstDiameters = new ListBox();
			chkLimitToQ34 = new CheckBox();
			lblHeader = new Label();
			chkIncludespacesHeaders = new CheckBox();
			chkNoCircles = new CheckBox();
			chk4Chords = new CheckBox();
			chkShapeNum = new CheckBox();
			cmbQcodes = new ComboBox();
			cmbShapeCodes = new ComboBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			panelGeneral = new Panel();
			lblModelFits = new Label();
			label5 = new Label();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelGeneral).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withDiametersToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)iRSateComprisonsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1184, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withDiametersToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)displayShapeModelDiametersToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAscsvToolStripMenuItem
			});
			((ToolStripItem)withDiametersToolStripMenuItem).set_Name("withDiametersToolStripMenuItem");
			((ToolStripItem)withDiametersToolStripMenuItem).set_Size(new Size(116, 20));
			((ToolStripItem)withDiametersToolStripMenuItem).set_Text("with Diameters...   ");
			((ToolStripItem)displayShapeModelDiametersToolStripMenuItem).set_Name("displayShapeModelDiametersToolStripMenuItem");
			((ToolStripItem)displayShapeModelDiametersToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)displayShapeModelDiametersToolStripMenuItem).set_Text("Display fitted Shape Model diameters");
			((ToolStripItem)displayShapeModelDiametersToolStripMenuItem).add_Click((EventHandler)displayShapeModelDiametersToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(273, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)262211);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy entire list");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveAscsvToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Name("saveAscsvToolStripMenuItem");
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Text("Save comparison with Satellites as .csv");
			((ToolStripItem)saveAscsvToolStripMenuItem).set_Visible(false);
			((ToolStripItem)saveAscsvToolStripMenuItem).add_Click((EventHandler)saveAscsvToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byQualityToolStripMenuItem,
				(ToolStripItem)byAsteroidToolStripMenuItem,
				(ToolStripItem)byAsteroidNumberShapeModelToolStripMenuItem,
				(ToolStripItem)byAsteroidNameToolStripMenuItem,
				(ToolStripItem)byMajorAxisToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(65, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byQualityToolStripMenuItem).set_Name("byQualityToolStripMenuItem");
			((ToolStripItem)byQualityToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byQualityToolStripMenuItem).set_Text("by Quality");
			((ToolStripItem)byQualityToolStripMenuItem).add_Click((EventHandler)byQualityToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidToolStripMenuItem).set_Name("byAsteroidToolStripMenuItem");
			((ToolStripItem)byAsteroidToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byAsteroidToolStripMenuItem).set_Text("by Asteroid number");
			((ToolStripItem)byAsteroidToolStripMenuItem).add_Click((EventHandler)byAsteroidToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNumberShapeModelToolStripMenuItem).set_Name("byAsteroidNumberShapeModelToolStripMenuItem");
			((ToolStripItem)byAsteroidNumberShapeModelToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byAsteroidNumberShapeModelToolStripMenuItem).set_Text("by Asteroid number + Shape model");
			((ToolStripItem)byAsteroidNumberShapeModelToolStripMenuItem).add_Click((EventHandler)byAsteroidNumberShapeModelToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Name("byAsteroidNameToolStripMenuItem");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Text("by Asteroid name");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).add_Click((EventHandler)byAsteroidNameToolStripMenuItem_Click);
			((ToolStripItem)byMajorAxisToolStripMenuItem).set_Name("byMajorAxisToolStripMenuItem");
			((ToolStripItem)byMajorAxisToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)byMajorAxisToolStripMenuItem).set_Text("by Major axis");
			((ToolStripItem)byMajorAxisToolStripMenuItem).add_Click((EventHandler)byMajorAxisToolStripMenuItem_Click);
			((ToolStripDropDownItem)iRSateComprisonsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)allTogetherToolStripMenuItem,
				(ToolStripItem)ellipseVsAllSatsToolStripMenuItem,
				(ToolStripItem)noneToolStripMenuItem
			});
			((ToolStripItem)iRSateComprisonsToolStripMenuItem).set_Name("iRSateComprisonsToolStripMenuItem");
			((ToolStripItem)iRSateComprisonsToolStripMenuItem).set_Size(new Size(158, 20));
			((ToolStripItem)iRSateComprisonsToolStripMenuItem).set_Text("IR-Satellite comparisons    ");
			((ToolStripItem)allTogetherToolStripMenuItem).set_Name("allTogetherToolStripMenuItem");
			((ToolStripItem)allTogetherToolStripMenuItem).set_Size(new Size(199, 22));
			((ToolStripItem)allTogetherToolStripMenuItem).set_Text("Shape model fits to All");
			((ToolStripItem)allTogetherToolStripMenuItem).add_Click((EventHandler)allTogetherToolStripMenuItem_Click);
			((ToolStripItem)ellipseVsAllSatsToolStripMenuItem).set_Name("ellipseVsAllSatsToolStripMenuItem");
			((ToolStripItem)ellipseVsAllSatsToolStripMenuItem).set_Size(new Size(199, 22));
			((ToolStripItem)ellipseVsAllSatsToolStripMenuItem).set_Text("All Ellipses vs Satellites");
			((ToolStripItem)ellipseVsAllSatsToolStripMenuItem).add_Click((EventHandler)ellipseVsAllSatsToolStripMenuItem_Click);
			((ToolStripItem)noneToolStripMenuItem).set_Name("noneToolStripMenuItem");
			((ToolStripItem)noneToolStripMenuItem).set_Size(new Size(199, 22));
			((ToolStripItem)noneToolStripMenuItem).set_Text("Return to normal diplay");
			((ToolStripItem)noneToolStripMenuItem).add_Click((EventHandler)noneToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstDiameters).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDiameters).set_FormattingEnabled(true);
			lstDiameters.set_HorizontalExtent(2000);
			lstDiameters.set_HorizontalScrollbar(true);
			lstDiameters.set_ItemHeight(14);
			((Control)lstDiameters).set_Location(new Point(5, 70));
			((Control)lstDiameters).set_Name("lstDiameters");
			lstDiameters.set_SelectionMode((SelectionMode)3);
			((Control)lstDiameters).set_Size(new Size(1174, 410));
			((Control)lstDiameters).set_TabIndex(1);
			((Control)lstDiameters).add_KeyDown(new KeyEventHandler(lstDiameters_KeyDown));
			((Control)lstDiameters).add_MouseDown(new MouseEventHandler(lstDiameters_MouseDown));
			((Control)chkLimitToQ34).set_AutoSize(true);
			((Control)chkLimitToQ34).set_Location(new Point(7, 5));
			((Control)chkLimitToQ34).set_Name("chkLimitToQ34");
			((Control)chkLimitToQ34).set_Size(new Size(167, 17));
			((Control)chkLimitToQ34).set_TabIndex(2);
			((Control)chkLimitToQ34).set_Text("Limit to Event Quality of 3 or 4");
			((ButtonBase)chkLimitToQ34).set_UseVisualStyleBackColor(true);
			chkLimitToQ34.add_CheckedChanged((EventHandler)chkLimitToQ34_CheckedChanged);
			((Control)lblHeader).set_AutoSize(true);
			((Control)lblHeader).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHeader).set_Location(new Point(7, 55));
			((Control)lblHeader).set_Name("lblHeader");
			((Control)lblHeader).set_Size(new Size(1456, 14));
			((Control)lblHeader).set_TabIndex(3);
			((Control)lblHeader).set_Text(componentResourceManager.GetString("lblHeader.Text"));
			((Control)chkIncludespacesHeaders).set_AutoSize(true);
			chkIncludespacesHeaders.set_Checked(true);
			chkIncludespacesHeaders.set_CheckState((CheckState)1);
			((Control)chkIncludespacesHeaders).set_Location(new Point(208, 22));
			((Control)chkIncludespacesHeaders).set_Name("chkIncludespacesHeaders");
			((Control)chkIncludespacesHeaders).set_Size(new Size(207, 17));
			((Control)chkIncludespacesHeaders).set_TabIndex(4);
			((Control)chkIncludespacesHeaders).set_Text("Include 5th-line breaks &&  Header lines");
			((ButtonBase)chkIncludespacesHeaders).set_UseVisualStyleBackColor(true);
			chkIncludespacesHeaders.add_CheckedChanged((EventHandler)chkIncludespacesHeaders_CheckedChanged);
			((Control)chkNoCircles).set_AutoSize(true);
			chkNoCircles.set_Checked(true);
			chkNoCircles.set_CheckState((CheckState)1);
			((Control)chkNoCircles).set_Location(new Point(208, 5));
			((Control)chkNoCircles).set_Name("chkNoCircles");
			((Control)chkNoCircles).set_Size(new Size(146, 17));
			((Control)chkNoCircles).set_TabIndex(5);
			((Control)chkNoCircles).set_Text("Exclude Circular solutions");
			((ButtonBase)chkNoCircles).set_UseVisualStyleBackColor(true);
			chkNoCircles.add_CheckedChanged((EventHandler)chkNoCircles_CheckedChanged);
			((Control)chk4Chords).set_AutoSize(true);
			((Control)chk4Chords).set_Location(new Point(7, 24));
			((Control)chk4Chords).set_Name("chk4Chords");
			((Control)chk4Chords).set_Size(new Size(84, 30));
			((Control)chk4Chords).set_TabIndex(6);
			((Control)chk4Chords).set_Text("Limit to 3 or \r\nmore chords");
			((ButtonBase)chk4Chords).set_UseVisualStyleBackColor(true);
			chk4Chords.add_CheckedChanged((EventHandler)chk4Chords_CheckedChanged);
			((Control)chkShapeNum).set_AutoSize(true);
			((Control)chkShapeNum).set_Location(new Point(106, 24));
			((Control)chkShapeNum).set_Name("chkShapeNum");
			((Control)chkShapeNum).set_Size(new Size(75, 30));
			((Control)chkShapeNum).set_TabIndex(7);
			((Control)chkShapeNum).set_Text("More than\r\none model");
			((ButtonBase)chkShapeNum).set_UseVisualStyleBackColor(true);
			chkShapeNum.add_CheckedChanged((EventHandler)chkShapeNum_CheckedChanged);
			((ListControl)cmbQcodes).set_FormattingEnabled(true);
			cmbQcodes.get_Items().AddRange(new object[8] { "0=Not fitted", "1=Bad occn data", "2=Model wrong", "3=Minimum dia", "4=Dia, but no fit", "5=Poor fit", "6=Good fit", "7=Not constrained" });
			((Control)cmbQcodes).set_Location(new Point(560, 26));
			((Control)cmbQcodes).set_Name("cmbQcodes");
			((Control)cmbQcodes).set_Size(new Size(133, 21));
			((Control)cmbQcodes).set_TabIndex(8);
			((ListControl)cmbShapeCodes).set_FormattingEnabled(true);
			cmbShapeCodes.get_Items().AddRange(new object[5] { "0=No reliable position or size", "1=Astrometry only. No reliable size", "2=Limits on size, but no shape", "3=Reliable size. Can fit to shape models", "4=Resolution better than shape models" });
			((Control)cmbShapeCodes).set_Location(new Point(481, 3));
			((Control)cmbShapeCodes).set_Name("cmbShapeCodes");
			((Control)cmbShapeCodes).set_Size(new Size(212, 21));
			((Control)cmbShapeCodes).set_TabIndex(9);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(432, 7));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(47, 13));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("Q codes");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(457, 30));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(101, 13));
			((Control)label2).set_TabIndex(11);
			((Control)label2).set_Text("Shape model codes");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_ForeColor(Color.DarkGreen);
			((Control)label3).set_Location(new Point(8, 33));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(164, 13));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("Ctrl + C will copy all selected lines");
			((Control)panelGeneral).get_Controls().Add((Control)(object)label5);
			((Control)panelGeneral).get_Controls().Add((Control)(object)label2);
			((Control)panelGeneral).get_Controls().Add((Control)(object)cmbQcodes);
			((Control)panelGeneral).get_Controls().Add((Control)(object)label1);
			((Control)panelGeneral).get_Controls().Add((Control)(object)cmbShapeCodes);
			((Control)panelGeneral).get_Controls().Add((Control)(object)chkShapeNum);
			((Control)panelGeneral).get_Controls().Add((Control)(object)chk4Chords);
			((Control)panelGeneral).get_Controls().Add((Control)(object)chkNoCircles);
			((Control)panelGeneral).get_Controls().Add((Control)(object)chkIncludespacesHeaders);
			((Control)panelGeneral).get_Controls().Add((Control)(object)chkLimitToQ34);
			((Control)panelGeneral).set_Location(new Point(481, 0));
			((Control)panelGeneral).set_Name("panelGeneral");
			((Control)panelGeneral).set_Size(new Size(697, 56));
			((Control)panelGeneral).set_TabIndex(13);
			((Control)lblModelFits).set_AutoSize(true);
			((Control)lblModelFits).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblModelFits).set_ForeColor(Color.MediumBlue);
			((Control)lblModelFits).set_Location(new Point(327, 31));
			((Control)lblModelFits).set_Name("lblModelFits");
			((Control)lblModelFits).set_Size(new Size(384, 20));
			((Control)lblModelFits).set_TabIndex(14);
			((Control)lblModelFits).set_Text("Best-fit diameters from fitting to Shape Models");
			((Control)lblModelFits).set_Visible(false);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.MistyRose);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(233, 39));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(222, 13));
			((Control)label5).set_TabIndex(19);
			((Control)label5).set_Text("Right-click on line to display the event");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(276, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1184, 488));
			((Control)this).get_Controls().Add((Control)(object)lblModelFits);
			((Control)this).get_Controls().Add((Control)(object)panelGeneral);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lblHeader);
			((Control)this).get_Controls().Add((Control)(object)lstDiameters);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterShowDias", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterShowDias);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(650, 120));
			((Control)this).set_Name("DisplayDiameters");
			((Control)this).set_Text("Display observed diameter values");
			((Form)this).add_Load((EventHandler)DisplayDiameters_Load);
			((Control)this).add_Resize((EventHandler)DisplayDiameters_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelGeneral).ResumeLayout(false);
			((Control)panelGeneral).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
