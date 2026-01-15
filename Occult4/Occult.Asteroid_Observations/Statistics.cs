using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Asteroid_Observations
{
	public class Statistics : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private int[] RUWE = new int[60];

		internal static List<RUWE_IDs> RUWE_ids = new List<RUWE_IDs>();

		private IContainer components;

		private Label label21;

		private Button cmdGetCoords;

		private Label label20;

		private Label label19;

		private Label label5;

		private TextBox txtH;

		private TextBox txtM;

		private TextBox txtDd;

		private TextBox txtS;

		private TextBox txtDm;

		private TextBox txtDs;

		private Label label22;

		private Label label23;

		private Label label18;

		private Panel panelURAT1;

		private Label label42;

		private TextBox txtUT1Number;

		private TextBox txtUT1Zone;

		private Panel panelUCAC4;

		private Label label41;

		private TextBox txtU4Number;

		private TextBox txtU4Zone;

		private Panel panelNOMAD;

		private Label label40;

		private TextBox txtNOMADnumber;

		private TextBox txtNOMADzone;

		private Panel panelUCAC3;

		private Label label39;

		private TextBox txtU3Number;

		private TextBox txtU3Zone;

		private Panel panelTycho2;

		private TextBox txtTycComp;

		private TextBox txtTycSeqNum;

		private TextBox txtTycRegion;

		private Label label15;

		private Label label16;

		private Panel panelUCAC;

		private TextBox txtUCAC;

		private Panel panelHip;

		private TextBox txtHip;

		private Panel panelB1;

		private Label label17;

		private TextBox txtB1number;

		private TextBox txtB1zone;

		private Label label3;

		private ComboBox cmbCatalogue;

		private Label label1;

		private Button cmdFindEvents;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private TextBox txtStatisticsList;

		private Button cmdFindMultipleEventStars;

		private Panel panel1;

		private Button cmdRegionalStats;

		private NumericUpDown updnYr1;

		private NumericUpDown updnYr2;

		private Label label6;

		private Label label7;

		private Button cmdAllObservers;

		private CheckBox chkByName;

		private CheckBox chkByNumber;

		private TextBox textBox1;

		private Panel panel2;

		private Button cmdListAsteroids;

		private Panel panel3;

		private Label label2;

		private Label label4;

		private CheckBox chkSortMagListByNumber;

		private CheckBox chkSaveIdentifiers;

		private ToolTip toolTip1;

		private Button cmdSmallMagDrops;

		private NumericUpDown updnDrop;

		private Label label8;

		private Button cmdSlowEvents;

		private Button cmdRUWEstats;

		private PictureBox picRUWE;

		private ToolStripMenuItem copyRUWEImageToolStripMenuItem;

		private ToolStripMenuItem copyRUWEEvents14ToolStripMenuItem;

		private NumericUpDown updnDiameter;

		private Label label9;

		private Button cmdSmallAsteroids;

		public Statistics()
		{
			InitializeComponent();
		}

		private void FindOccultedStars_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)txtTycComp).set_Text("1");
			((Control)this).set_Height(600);
			NumericUpDown obj = updnYr1;
			decimal value;
			updnYr2.set_Value(value = DateTime.Now.Year);
			obj.set_Value(value);
			chkByName.set_Checked(true);
			((Control)picRUWE).set_Visible(false);
			NumericUpDown obj2 = updnYr1;
			updnYr2.set_Maximum(value = DateTime.Now.Year);
			obj2.set_Maximum(value);
		}

		private void cmdGetCoords_Click(object sender, EventArgs e)
		{
			//IL_0112: Unknown result type (might be due to invalid IL or missing references)
			double RA = 0.0;
			double pmRA = 0.0;
			double Dec = 0.0;
			double pmDec = 0.0;
			double Parallax = 0.0;
			double MagV = 0.0;
			double MagB = 0.0;
			double MagR = 0.0;
			double Epoch = 2000.0;
			bool flag = false;
			bool UsedGaia = false;
			int result = 0;
			int result2 = 0;
			PictureBox obj = picRUWE;
			bool UsedGaia2;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(UsedGaia2 = false);
			((Control)obj).set_Visible(UsedGaia2);
			int result5;
			double Parallax_asec;
			double Epoch2;
			int GaiaVersion;
			ulong GaiaSourceID;
			double RadialVelocity;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarDiameter_mas;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			string SourceFile;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				if (!int.TryParse(((Control)txtHip).get_Text(), out var result3))
				{
					result3 = 0;
				}
				flag = GetStarPosition.GetHipparcosPosition(result3, out RA, out Dec, out pmRA, out pmDec, out MagR, out MagV, out MagB, out Parallax, out Epoch, out UsedGaia, out var CorrectStarIdentifier);
				if (!flag)
				{
					MessageBox.Show("HIP" + ((Control)txtHip).get_Text() + " is held in the Occult Gaia catalogue as\r\n" + CorrectStarIdentifier + "\r\nSet the star identifier to this star", "HIP star identifier", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				if (!int.TryParse(((Control)txtTycRegion).get_Text(), out result))
				{
					result = 0;
				}
				if (!int.TryParse(((Control)txtTycSeqNum).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(((Control)txtTycComp).get_Text(), out var result4))
				{
					result4 = 0;
				}
				flag = GetStarPosition.GetTycho2Position(result, result2, result4, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				if (!int.TryParse(((Control)txtU4Zone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtU4Number).get_Text(), out result2))
				{
					result2 = 0;
				}
				flag = GetStarPosition.GetUCAC4Position(result5, result2, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 3)
			{
				if (!int.TryParse(((Control)txtB1zone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtB1number).get_Text(), out result2))
				{
					result2 = 0;
				}
				flag = GetStarPosition.GetB1Position(result5, result2, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch2, out UsedGaia2, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 4)
			{
				if (!int.TryParse(((Control)txtNOMADzone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtNOMADnumber).get_Text(), out result2))
				{
					result2 = 0;
				}
				flag = GetStarPosition.GetNOMAD_Full_Position(result5, result2, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out StarReliability, out StarDiameter_mas, out UsedGaia2, out GaiaPMfromUCAC, out GaiaSourceID, out SourceFile, out UncertPMDec, out UncertPMRA, out UncertDec, out UncertRA, out RadialVelocity, out Epoch2, out Parallax_asec, out NoGaiaPM, out DuplicateSource, out GaiaVersion);
			}
			if (!flag)
			{
				TextBox obj2 = txtH;
				TextBox obj3 = txtM;
				TextBox obj4 = txtS;
				TextBox obj5 = txtDd;
				TextBox obj6 = txtDm;
				string text;
				((Control)txtDs).set_Text(text = "");
				string text2;
				((Control)obj6).set_Text(text2 = text);
				string text3;
				((Control)obj5).set_Text(text3 = text2);
				string text4;
				((Control)obj4).set_Text(text4 = text3);
				((Control)obj3).set_Text(SourceFile = text4);
				((Control)obj2).set_Text(SourceFile);
			}
			else
			{
				string text5 = Utilities.DEGtoDMS(RA / 15.0 * (180.0 / Math.PI), 2, 2, MinutesOnly: false);
				((Control)txtH).set_Text(text5.Substring(0, 2));
				((Control)txtM).set_Text(text5.Substring(3, 2));
				((Control)txtS).set_Text(text5.Substring(6));
				text5 = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1, MinutesOnly: false);
				((Control)txtDd).set_Text(text5.Substring(0, 3));
				((Control)txtDm).set_Text(text5.Substring(4, 2));
				((Control)txtDs).set_Text(text5.Substring(7));
				Application.DoEvents();
				FindEvents();
			}
		}

		private void cmbCatalogue_SelectedIndexChanged(object sender, EventArgs e)
		{
			ShowNumberField(((ListControl)cmbCatalogue).get_SelectedIndex());
		}

		private void ShowNumberField(int x)
		{
			((Control)panelHip).set_Visible(x == 0);
			((Control)panelTycho2).set_Visible(x == 1);
			((Control)panelUCAC4).set_Visible(x == 2);
			((Control)panelUCAC3).set_Visible(x == 3);
			((Control)panelUCAC).set_Visible(x == 4);
			((Control)panelB1).set_Visible(x == 5);
			((Control)panelNOMAD).set_Visible(x == 6);
			((Control)panelURAT1).set_Visible(x == 7);
		}

		private void cmdFindEvents_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			FindEvents();
		}

		private void FindEvents()
		{
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			double num = 0.0;
			double num2 = 0.0;
			if (!int.TryParse(((Control)txtH).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtM).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtS).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			num = ((double)result + (double)result2 / 60.0 + result3 / 3600.0) * 15.0 / (180.0 / Math.PI);
			if (num == 0.0)
			{
				MessageBox.Show("The star's RA and Declination have not been specified", "No star coords", (MessageBoxButtons)0);
				return;
			}
			if (!int.TryParse(((Control)txtDd).get_Text().Replace("-", ""), out var result4))
			{
				result4 = 0;
			}
			if (!int.TryParse(((Control)txtDm).get_Text(), out var result5))
			{
				result5 = 0;
			}
			if (!double.TryParse(((Control)txtDs).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			num2 = ((double)result4 + (double)result5 / 60.0 + result6 / 3600.0) / (180.0 / Math.PI);
			if (((Control)txtDd).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.FindAsteroidOccultedStars(num, num2, out var _, out var _));
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)txtStatisticsList).get_Text());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(((Control)txtStatisticsList).get_Text());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(((Control)txtStatisticsList).get_Text(), "Events by a star", Settings.Default.Save_AsteroidResults);
		}

		private void cmdFindMultipleEventStars_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.FindDuplicatedStars_plus_MagStats(chkSortMagListByNumber.get_Checked()));
		}

		private void FindOccultedStars_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(875);
			if (((Control)this).get_Height() < 400)
			{
				((Control)this).set_Height(400);
			}
			((Control)txtStatisticsList).set_Height(((Control)this).get_Height() - 270);
			((Control)picRUWE).set_Left(((Control)txtStatisticsList).get_Left());
			((Control)picRUWE).set_Top(((Control)txtStatisticsList).get_Top());
			((Control)picRUWE).set_Height(((Control)txtStatisticsList).get_Height());
			((Control)picRUWE).set_Width(((Control)txtStatisticsList).get_Width());
			if (((Control)picRUWE).get_Visible())
			{
				PlotRUWE();
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"FindByStar");
		}

		private void cmdRegionalStats_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.GetRegionalStats((int)updnYr1.get_Value(), (int)updnYr2.get_Value(), chkSaveIdentifiers.get_Checked()));
		}

		private void updnYr1_ValueChanged(object sender, EventArgs e)
		{
			if (updnYr1.get_Value() > updnYr2.get_Value())
			{
				updnYr2.set_Value(updnYr1.get_Value());
			}
		}

		private void updnYr2_ValueChanged(object sender, EventArgs e)
		{
			if (updnYr2.get_Value() < updnYr1.get_Value())
			{
				updnYr1.set_Value(updnYr2.get_Value());
			}
		}

		private void cmdAllObservers_Click(object sender, EventArgs e)
		{
			GetObservers(ExcludeStarsWithNoPM: false);
		}

		private void GetObservers(bool ExcludeStarsWithNoPM)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			Asteroid_Observations_Reports.PositionsList.Clear();
			Asteroid_Observations_Reports.ObserverList.Clear();
			int num = -1;
			int[] array = new int[7];
			string[] array2 = new string[7] { "Australasia ", "East Asia   ", "Europe      ", "Nth America ", "Sth America ", "Other       ", "World       " };
			AllEvents allEvents = new AllEvents();
			allEvents.ReadObservationsFile(HistoricalFile: true, "");
			allEvents.GetPositionsAndObservers(WithNames: true, Gaia_DR2_Updates: false, Asteroids: true, Planets: true, AsteroidSatellites: true, IncludeAlreadyReported: true, (int)updnYr1.get_Value(), (int)updnYr2.get_Value(), 3000000.0, 0, "", ObserverOnly: false, ExcludeStarsWithNoPM, IncludeExtraData: false);
			allEvents.OccEvents.Clear();
			Asteroid_Observations_Reports.ObserverCount.Clear();
			for (int num2 = Asteroid_Observations_Reports.ObserverList.Count - 1; num2 >= 0; num2--)
			{
				Asteroid_Observations_Reports.ObserverList[num2].ObserverID = Asteroid_Observations_Reports.ObserverList[num2].ObserverID.ToString().Replace("{", "").Replace("}", "");
				if (Asteroid_Observations_Reports.ObserverList[num2].ObserverID.Length < 2)
				{
					Asteroid_Observations_Reports.ObserverList[num2].ObserverID = "_. " + Asteroid_Observations_Reports.ObserverList[num2].ObserverID;
				}
				else if (!Asteroid_Observations_Reports.ObserverList[num2].ObserverID.Substring(0, 2).Contains("."))
				{
					Asteroid_Observations_Reports.ObserverList[num2].ObserverID = "_. " + Asteroid_Observations_Reports.ObserverList[num2].ObserverID.Trim();
				}
				if (Asteroid_Observations_Reports.ObserverList[num2].ObserverID.ToString().Contains("(") | Asteroid_Observations_Reports.ObserverList[num2].ObserverID.ToString().Contains(")"))
				{
					Asteroid_Observations_Reports.ObserverList.RemoveAt(num2);
				}
				int num3 = Asteroid_Observations_Reports.ObserverList[num2].ObserverID.ToString().IndexOf(".");
				if (num3 > 0 && Asteroid_Observations_Reports.ObserverList[num2].ObserverID.ToString().Substring(num3 + 1).Trim()
					.Length < 2)
				{
					Asteroid_Observations_Reports.ObserverList.RemoveAt(num2);
				}
			}
			for (int num4 = Asteroid_Observations_Reports.ObserverList.Count - 1; num4 >= 0; num4--)
			{
				Observers observers = new Observers();
				observers.Observer = Asteroid_Observations_Reports.ObserverList[num4].ObserverID;
				observers.Region = Asteroid_Observations_Reports.ObserverList[num4].Region;
				observers.Count = 1;
				Asteroid_Observations_Reports.ObserverCount.Add(observers);
			}
			Observers.Sort = 1;
			Asteroid_Observations_Reports.ObserverCount.Sort();
			for (int num5 = Asteroid_Observations_Reports.ObserverCount.Count - 1; num5 > 0; num5--)
			{
				if (Asteroid_Observations_Reports.ObserverCount[num5].Observer.ToString() == Asteroid_Observations_Reports.ObserverCount[num5 - 1].Observer.ToString())
				{
					Asteroid_Observations_Reports.ObserverCount[num5 - 1].Count += Asteroid_Observations_Reports.ObserverCount[num5].Count;
					Asteroid_Observations_Reports.ObserverCount.RemoveAt(num5);
				}
			}
			if (Asteroid_Observations_Reports.ObserverCount[0].Observer.Trim() == "")
			{
				Asteroid_Observations_Reports.ObserverCount.RemoveAt(0);
			}
			Observers.Sort = 2;
			Asteroid_Observations_Reports.ObserverCount.Sort();
			if (chkByNumber.get_Checked())
			{
				Observers.Sort = 0;
				Asteroid_Observations_Reports.ObserverCount.Sort();
			}
			for (int i = 0; i < Asteroid_Observations_Reports.ObserverCount.Count; i++)
			{
				num = Asteroid_Observations_Reports.ObserverCount[i].Region;
				if (num >= 0)
				{
					array[num]++;
					array[6]++;
				}
			}
			string text = "Number of observers by region  -  " + updnYr1.get_Value() + " to " + updnYr2.get_Value() + "\r\n";
			for (int j = 0; j < 7; j++)
			{
				text = text + array2[j] + ":  " + array[j] + "\r\n";
			}
			text += "\r\n\r\nObservers sorted by ";
			text = ((!chkByNumber.get_Checked()) ? (text + "family name") : (text + "the number of observations"));
			text = text + "  -  " + updnYr1.get_Value() + " to " + updnYr2.get_Value() + "\r\n";
			for (int k = 0; k < Asteroid_Observations_Reports.ObserverCount.Count; k++)
			{
				if (k % 5 == 0)
				{
					text += "\r\n";
				}
				text = text + Asteroid_Observations_Reports.ObserverCount[k].Observer.ToString().PadRight(20) + Asteroid_Observations_Reports.ObserverCount[k].Count.ToString().PadLeft(4) + "\r\n";
			}
			((Control)txtStatisticsList).set_Text(text);
		}

		private void chkByName_MouseClick(object sender, MouseEventArgs e)
		{
			chkByName.set_Checked(true);
			chkByNumber.set_Checked(!chkByName.get_Checked());
			GetObservers(ExcludeStarsWithNoPM: false);
		}

		private void chkByNumber_MouseClick(object sender, MouseEventArgs e)
		{
			chkByNumber.set_Checked(true);
			chkByName.set_Checked(!chkByNumber.get_Checked());
			GetObservers(ExcludeStarsWithNoPM: false);
		}

		private void cmdListAsteroids_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.FindAsterodsInOccultations());
		}

		private void cmdSmallMagDrops_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.SmallMagDropEvents((double)updnDrop.get_Value()));
		}

		private void updnDrop_ValueChanged(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.SmallMagDropEvents((double)updnDrop.get_Value()));
		}

		private void cmdSlowEvents_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.GetSlowEvents());
		}

		private void cmdRUWEstats_Click(object sender, EventArgs e)
		{
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.GetRUWEcounts((int)updnYr1.get_Value(), (int)updnYr2.get_Value(), out RUWE);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("Number of events\r\nby RUWE,\r\nfor the period:\r\n{0,4} to {1,4}\r\n\r\n", (int)updnYr1.get_Value(), (int)updnYr2.get_Value());
			stringBuilder.Append(" RUWE  Count\r\n");
			string text = " ";
			for (int i = 10; i < RUWE.Length; i++)
			{
				if (i == RUWE.Length - 1)
				{
					text = "+";
				}
				stringBuilder.AppendFormat("{0,5:f2}" + text + " {1,4}\r\n", (double)i / 20.0, RUWE[i]);
			}
			((Control)txtStatisticsList).set_Text(stringBuilder.ToString());
			PlotRUWE();
		}

		private void copyRUWEEvents14ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < RUWE_ids.Count; i++)
			{
				text += RUWE_ids[i].ToString();
			}
			Clipboard.SetText(text);
		}

		private void PlotRUWE()
		{
			((Control)picRUWE).set_Left(((Control)txtStatisticsList).get_Left() + 105);
			((Control)picRUWE).set_Top(((Control)txtStatisticsList).get_Top());
			((Control)picRUWE).set_Height(((Control)txtStatisticsList).get_Height());
			((Control)picRUWE).set_Width(((Control)txtStatisticsList).get_Width() - 105);
			((Control)picRUWE).set_Visible(true);
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(true);
			int width = ((Control)picRUWE).get_Width();
			int height = ((Control)picRUWE).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.SmoothingMode = SmoothingMode.AntiAlias;
			graphics.Clear(Color.LemonChiffon);
			Pen pen = new Pen(Color.Black, 1.5f);
			Pen pen2 = new Pen(Color.LightGray, 0.5f);
			Brush brush = new SolidBrush(Color.LightBlue);
			Brush brush2 = new SolidBrush(Color.Tan);
			Brush brush3 = new SolidBrush(Color.Coral);
			Font font = new Font("Courier", 8.25f, FontStyle.Regular);
			Font font2 = new Font("Ariel", 14f, FontStyle.Regular);
			Brush brush4 = new SolidBrush(Color.Black);
			int num = 0;
			int num2 = 10;
			int num3 = 60;
			for (int i = 0; i < RUWE.Length; i++)
			{
				if (RUWE[i] > num)
				{
					num = RUWE[i];
				}
			}
			num = (int)((double)num * 1.1);
			int num4 = 10;
			int num5 = height - 30;
			int num6 = 50;
			int num7 = width - 50;
			float num8 = (float)num / (float)(num4 - num5);
			float num9 = (float)(num7 - num6) / (float)(num3 - num2);
			float num10;
			for (int j = num2; j <= num3; j++)
			{
				if (j % 2 == 0)
				{
					graphics.DrawLine(pen, (float)num6 + num9 / 2f + (float)(j - num2) * num9, num5, (float)num6 + num9 / 2f + (float)(j - num2) * num9, num5 + 1);
					string text = string.Format("{0,1:f1}", (double)j / 20.0);
					num10 = graphics.MeasureString(text, font).Width / 2f;
					graphics.DrawString(text, font, brush4, (float)num6 + num9 / 2f + (float)(j - num2) * num9 - num10, num5 + 5);
				}
			}
			int num11 = num / 8;
			num11 = ((num11 > 1000) ? (1000 * (num11 / 1000)) : ((num11 > 100) ? (100 * (num11 / 100)) : ((num11 <= 10) ? 1 : (10 * (num11 / 10)))));
			int num12 = 0;
			if (num8 != 0f)
			{
				for (int k = 0; k <= num; k += num11)
				{
					float num13 = (float)num5 + (float)k / num8;
					graphics.DrawLine(pen, num6 - 4, num13, num6, num13);
					graphics.DrawLine(pen2, num6, num13, (float)num7 + num9, num13);
					string text2 = string.Format("{0,1:f0}", num11 * num12);
					num10 = graphics.MeasureString(text2, font).Width;
					graphics.DrawString(text2, font, brush4, (float)num6 - num10 - 4f, num13 - 7f);
					num12++;
				}
			}
			string text3 = string.Format("Number of events by RUWE, for the period {0,4} to {1,4}\r\n", (int)updnYr1.get_Value(), (int)updnYr2.get_Value());
			num10 = graphics.MeasureString(text3, font2).Width / 2f;
			graphics.DrawString(text3, font2, brush4, (float)(width / 2) - num10, 4f);
			for (int l = num2; l <= num3; l++)
			{
				float num14 = (float)RUWE[l] / num8;
				if (l < 28)
				{
					graphics.FillRectangle(brush, (float)num6 + (float)(l - num2) * num9, (float)num5 + num14, num9, 0f - num14);
				}
				else if (l == 28)
				{
					graphics.FillRectangle(brush2, (float)num6 + (float)(l - num2) * num9, (float)num5 + num14, num9, 0f - num14);
				}
				else
				{
					graphics.FillRectangle(brush3, (float)num6 + (float)(l - num2) * num9, (float)num5 + num14, num9, 0f - num14);
				}
				graphics.DrawRectangle(pen, (float)num6 + (float)(l - num2) * num9, (float)num5 + num14, num9, 0f - num14);
			}
			graphics.DrawLine(pen, num6, num5, num7, num5);
			graphics.DrawLine(pen, num6, num5, num6, num4);
			picRUWE.set_Image((Image)image);
			graphics.Dispose();
		}

		private void copyRUWEImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picRUWE.get_Image());
		}

		private void cmdSmallAsteroids_Click(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.SmallDiameterEvents((double)updnDiameter.get_Value()));
		}

		private void updnDiameter_ValueChanged(object sender, EventArgs e)
		{
			PictureBox obj = picRUWE;
			bool visible;
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			((Control)txtStatisticsList).set_Text(Asteroid_Observations_Reports.SmallDiameterEvents((double)updnDiameter.get_Value()));
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
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b8: Expected O, but got Unknown
			//IL_03b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c3: Expected O, but got Unknown
			//IL_2042: Unknown result type (might be due to invalid IL or missing references)
			//IL_246c: Unknown result type (might be due to invalid IL or missing references)
			//IL_2476: Expected O, but got Unknown
			//IL_250c: Unknown result type (might be due to invalid IL or missing references)
			//IL_2516: Expected O, but got Unknown
			components = new Container();
			label21 = new Label();
			cmdGetCoords = new Button();
			label20 = new Label();
			label19 = new Label();
			label5 = new Label();
			txtH = new TextBox();
			txtM = new TextBox();
			txtDd = new TextBox();
			txtS = new TextBox();
			txtDm = new TextBox();
			txtDs = new TextBox();
			label22 = new Label();
			label23 = new Label();
			label1 = new Label();
			label18 = new Label();
			panelURAT1 = new Panel();
			label42 = new Label();
			txtUT1Number = new TextBox();
			txtUT1Zone = new TextBox();
			panelUCAC4 = new Panel();
			label41 = new Label();
			txtU4Number = new TextBox();
			txtU4Zone = new TextBox();
			panelNOMAD = new Panel();
			label40 = new Label();
			txtNOMADnumber = new TextBox();
			txtNOMADzone = new TextBox();
			panelUCAC3 = new Panel();
			label39 = new Label();
			txtU3Number = new TextBox();
			txtU3Zone = new TextBox();
			panelTycho2 = new Panel();
			txtTycComp = new TextBox();
			txtTycSeqNum = new TextBox();
			txtTycRegion = new TextBox();
			label15 = new Label();
			label16 = new Label();
			panelUCAC = new Panel();
			txtUCAC = new TextBox();
			panelHip = new Panel();
			txtHip = new TextBox();
			panelB1 = new Panel();
			label17 = new Label();
			txtB1number = new TextBox();
			txtB1zone = new TextBox();
			label3 = new Label();
			cmbCatalogue = new ComboBox();
			cmdFindEvents = new Button();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyRUWEImageToolStripMenuItem = new ToolStripMenuItem();
			copyRUWEEvents14ToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			txtStatisticsList = new TextBox();
			cmdFindMultipleEventStars = new Button();
			panel1 = new Panel();
			textBox1 = new TextBox();
			cmdRegionalStats = new Button();
			updnYr1 = new NumericUpDown();
			updnYr2 = new NumericUpDown();
			label6 = new Label();
			label7 = new Label();
			cmdAllObservers = new Button();
			chkByName = new CheckBox();
			chkByNumber = new CheckBox();
			panel2 = new Panel();
			updnDiameter = new NumericUpDown();
			label9 = new Label();
			cmdSmallAsteroids = new Button();
			updnDrop = new NumericUpDown();
			cmdSmallMagDrops = new Button();
			chkSortMagListByNumber = new CheckBox();
			panel3 = new Panel();
			cmdRUWEstats = new Button();
			cmdSlowEvents = new Button();
			chkSaveIdentifiers = new CheckBox();
			label2 = new Label();
			label4 = new Label();
			cmdListAsteroids = new Button();
			label8 = new Label();
			toolTip1 = new ToolTip(components);
			picRUWE = new PictureBox();
			((Control)panelURAT1).SuspendLayout();
			((Control)panelUCAC4).SuspendLayout();
			((Control)panelNOMAD).SuspendLayout();
			((Control)panelUCAC3).SuspendLayout();
			((Control)panelTycho2).SuspendLayout();
			((Control)panelUCAC).SuspendLayout();
			((Control)panelHip).SuspendLayout();
			((Control)panelB1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnYr1).BeginInit();
			((ISupportInitialize)updnYr2).BeginInit();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)updnDiameter).BeginInit();
			((ISupportInitialize)updnDrop).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)picRUWE).BeginInit();
			((Control)this).SuspendLayout();
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(135, 130));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(13, 13));
			((Control)label21).set_TabIndex(48);
			((Control)label21).set_Text("o");
			((Control)cmdGetCoords).set_BackColor(Color.GreenYellow);
			((Control)cmdGetCoords).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetCoords).set_Location(new Point(17, 90));
			((Control)cmdGetCoords).set_Name("cmdGetCoords");
			((Control)cmdGetCoords).set_Size(new Size(200, 26));
			((Control)cmdGetCoords).set_TabIndex(0);
			((Control)cmdGetCoords).set_Text("Get coords from Star ID, and find events");
			((ButtonBase)cmdGetCoords).set_UseVisualStyleBackColor(false);
			((Control)cmdGetCoords).add_Click((EventHandler)cmdGetCoords_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(90, 130));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(12, 13));
			((Control)label20).set_TabIndex(47);
			((Control)label20).set_Text("s");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(70, 130));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(15, 13));
			((Control)label19).set_TabIndex(46);
			((Control)label19).set_Text("m");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(55, 130));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(45);
			((Control)label5).set_Text("h");
			((Control)txtH).set_Location(new Point(46, 143));
			((Control)txtH).set_Name("txtH");
			((Control)txtH).set_Size(new Size(20, 20));
			((Control)txtH).set_TabIndex(1);
			((Control)txtM).set_Location(new Point(66, 143));
			((Control)txtM).set_Name("txtM");
			((Control)txtM).set_Size(new Size(18, 20));
			((Control)txtM).set_TabIndex(2);
			((Control)txtDd).set_Location(new Point(126, 143));
			((Control)txtDd).set_Name("txtDd");
			((Control)txtDd).set_Size(new Size(21, 20));
			((Control)txtDd).set_TabIndex(4);
			((Control)txtS).set_Location(new Point(84, 143));
			((Control)txtS).set_Name("txtS");
			((Control)txtS).set_Size(new Size(35, 20));
			((Control)txtS).set_TabIndex(3);
			((Control)txtDm).set_Location(new Point(147, 143));
			((Control)txtDm).set_Name("txtDm");
			((Control)txtDm).set_Size(new Size(18, 20));
			((Control)txtDm).set_TabIndex(5);
			((Control)txtDs).set_Location(new Point(165, 143));
			((Control)txtDs).set_Name("txtDs");
			((Control)txtDs).set_Size(new Size(31, 20));
			((Control)txtDs).set_TabIndex(6);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(156, 135));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(9, 13));
			((Control)label22).set_TabIndex(49);
			((Control)label22).set_Text("'");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(174, 135));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(12, 13));
			((Control)label23).set_TabIndex(50);
			((Control)label23).set_Text("\"");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(49, 120));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(149, 13));
			((Control)label1).set_TabIndex(51);
			((Control)label1).set_Text("OR enter the star coordinates,");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(10, 56));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(44, 13));
			((Control)label18).set_TabIndex(29);
			((Control)label18).set_Text("Number");
			((Control)panelURAT1).get_Controls().Add((Control)(object)label42);
			((Control)panelURAT1).get_Controls().Add((Control)(object)txtUT1Number);
			((Control)panelURAT1).get_Controls().Add((Control)(object)txtUT1Zone);
			((Control)panelURAT1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelURAT1).set_Location(new Point(54, 48));
			((Control)panelURAT1).set_Name("panelURAT1");
			((Control)panelURAT1).set_Size(new Size(141, 33));
			((Control)panelURAT1).set_TabIndex(37);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(48, 10));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(11, 13));
			((Control)label42).set_TabIndex(2);
			((Control)label42).set_Text("-");
			((Control)txtUT1Number).set_Location(new Point(60, 7));
			((Control)txtUT1Number).set_Name("txtUT1Number");
			((Control)txtUT1Number).set_Size(new Size(70, 20));
			((Control)txtUT1Number).set_TabIndex(1);
			((Control)txtUT1Zone).set_Location(new Point(16, 7));
			((Control)txtUT1Zone).set_Name("txtUT1Zone");
			((Control)txtUT1Zone).set_Size(new Size(31, 20));
			((Control)txtUT1Zone).set_TabIndex(0);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)label41);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Number);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Zone);
			((Control)panelUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC4).set_Location(new Point(54, 51));
			((Control)panelUCAC4).set_Name("panelUCAC4");
			((Control)panelUCAC4).set_Size(new Size(141, 33));
			((Control)panelUCAC4).set_TabIndex(36);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(48, 10));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(11, 13));
			((Control)label41).set_TabIndex(2);
			((Control)label41).set_Text("-");
			((Control)txtU4Number).set_Location(new Point(60, 7));
			((Control)txtU4Number).set_Name("txtU4Number");
			((Control)txtU4Number).set_Size(new Size(70, 20));
			((Control)txtU4Number).set_TabIndex(1);
			((Control)txtU4Zone).set_Location(new Point(16, 7));
			((Control)txtU4Zone).set_Name("txtU4Zone");
			((Control)txtU4Zone).set_Size(new Size(31, 20));
			((Control)txtU4Zone).set_TabIndex(0);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)label40);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADnumber);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADzone);
			((Control)panelNOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelNOMAD).set_Location(new Point(54, 53));
			((Control)panelNOMAD).set_Name("panelNOMAD");
			((Control)panelNOMAD).set_Size(new Size(141, 33));
			((Control)panelNOMAD).set_TabIndex(35);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(48, 10));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(11, 13));
			((Control)label40).set_TabIndex(2);
			((Control)label40).set_Text("-");
			((Control)txtNOMADnumber).set_Location(new Point(60, 7));
			((Control)txtNOMADnumber).set_Name("txtNOMADnumber");
			((Control)txtNOMADnumber).set_Size(new Size(70, 20));
			((Control)txtNOMADnumber).set_TabIndex(1);
			((Control)txtNOMADzone).set_Location(new Point(16, 7));
			((Control)txtNOMADzone).set_Name("txtNOMADzone");
			((Control)txtNOMADzone).set_Size(new Size(31, 20));
			((Control)txtNOMADzone).set_TabIndex(0);
			((Control)panelUCAC3).get_Controls().Add((Control)(object)label39);
			((Control)panelUCAC3).get_Controls().Add((Control)(object)txtU3Number);
			((Control)panelUCAC3).get_Controls().Add((Control)(object)txtU3Zone);
			((Control)panelUCAC3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC3).set_Location(new Point(54, 53));
			((Control)panelUCAC3).set_Name("panelUCAC3");
			((Control)panelUCAC3).set_Size(new Size(141, 33));
			((Control)panelUCAC3).set_TabIndex(34);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(48, 10));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(11, 13));
			((Control)label39).set_TabIndex(2);
			((Control)label39).set_Text("-");
			((Control)txtU3Number).set_Location(new Point(60, 7));
			((Control)txtU3Number).set_Name("txtU3Number");
			((Control)txtU3Number).set_Size(new Size(70, 20));
			((Control)txtU3Number).set_TabIndex(1);
			((Control)txtU3Zone).set_Location(new Point(16, 7));
			((Control)txtU3Zone).set_Name("txtU3Zone");
			((Control)txtU3Zone).set_Size(new Size(31, 20));
			((Control)txtU3Zone).set_TabIndex(0);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycComp);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycSeqNum);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycRegion);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label15);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label16);
			((Control)panelTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelTycho2).set_Location(new Point(54, 53));
			((Control)panelTycho2).set_Name("panelTycho2");
			((Control)panelTycho2).set_Size(new Size(142, 33));
			((Control)panelTycho2).set_TabIndex(33);
			((Control)txtTycComp).set_Location(new Point(111, 7));
			((Control)txtTycComp).set_Name("txtTycComp");
			((Control)txtTycComp).set_Size(new Size(17, 20));
			((Control)txtTycComp).set_TabIndex(2);
			((Control)txtTycSeqNum).set_Location(new Point(56, 7));
			((Control)txtTycSeqNum).set_Name("txtTycSeqNum");
			((Control)txtTycSeqNum).set_Size(new Size(47, 20));
			((Control)txtTycSeqNum).set_TabIndex(1);
			((Control)txtTycRegion).set_Location(new Point(8, 7));
			((Control)txtTycRegion).set_Name("txtTycRegion");
			((Control)txtTycRegion).set_Size(new Size(39, 20));
			((Control)txtTycRegion).set_TabIndex(0);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(101, 10));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(11, 13));
			((Control)label15).set_TabIndex(0);
			((Control)label15).set_Text("-");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(46, 10));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(11, 13));
			((Control)label16).set_TabIndex(4);
			((Control)label16).set_Text("-");
			((Control)panelUCAC).get_Controls().Add((Control)(object)txtUCAC);
			((Control)panelUCAC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC).set_Location(new Point(54, 53));
			((Control)panelUCAC).set_Name("panelUCAC");
			((Control)panelUCAC).set_Size(new Size(141, 33));
			((Control)panelUCAC).set_TabIndex(31);
			((Control)txtUCAC).set_Location(new Point(25, 7));
			((Control)txtUCAC).set_Name("txtUCAC");
			((Control)txtUCAC).set_Size(new Size(70, 20));
			((Control)txtUCAC).set_TabIndex(0);
			((Control)panelHip).get_Controls().Add((Control)(object)txtHip);
			((Control)panelHip).set_Location(new Point(54, 53));
			((Control)panelHip).set_Name("panelHip");
			((Control)panelHip).set_Size(new Size(141, 33));
			((Control)panelHip).set_TabIndex(30);
			((Control)txtHip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHip).set_Location(new Point(25, 7));
			((Control)txtHip).set_Name("txtHip");
			((Control)txtHip).set_Size(new Size(70, 20));
			((Control)txtHip).set_TabIndex(2);
			((Control)panelB1).get_Controls().Add((Control)(object)label17);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1number);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1zone);
			((Control)panelB1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelB1).set_Location(new Point(54, 53));
			((Control)panelB1).set_Name("panelB1");
			((Control)panelB1).set_Size(new Size(141, 33));
			((Control)panelB1).set_TabIndex(32);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(48, 10));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(11, 13));
			((Control)label17).set_TabIndex(2);
			((Control)label17).set_Text("-");
			((Control)txtB1number).set_Location(new Point(60, 7));
			((Control)txtB1number).set_Name("txtB1number");
			((Control)txtB1number).set_Size(new Size(70, 20));
			((Control)txtB1number).set_TabIndex(1);
			((Control)txtB1zone).set_Location(new Point(8, 7));
			((Control)txtB1zone).set_Name("txtB1zone");
			((Control)txtB1zone).set_Size(new Size(39, 20));
			((Control)txtB1zone).set_TabIndex(0);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(11, 23));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(55, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Catalogue");
			cmbCatalogue.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbCatalogue).set_FormattingEnabled(true);
			cmbCatalogue.get_Items().AddRange(new object[8] { "Hipparcos", "Tycho-2", "UCAC4", "UCAC3", "UCAC2", "USNO-B1", "NOMAD", "URAT1" });
			((Control)cmbCatalogue).set_Location(new Point(72, 19));
			((Control)cmbCatalogue).set_Name("cmbCatalogue");
			((Control)cmbCatalogue).set_Size(new Size(105, 21));
			((Control)cmbCatalogue).set_TabIndex(1);
			cmbCatalogue.add_SelectedIndexChanged((EventHandler)cmbCatalogue_SelectedIndexChanged);
			((Control)cmdFindEvents).set_Location(new Point(54, 165));
			((Control)cmdFindEvents).set_Name("cmdFindEvents");
			((Control)cmdFindEvents).set_Size(new Size(134, 24));
			((Control)cmdFindEvents).set_TabIndex(2);
			((Control)cmdFindEvents).set_Text("Find events for this star");
			((ButtonBase)cmdFindEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdFindEvents).add_Click((EventHandler)cmdFindEvents_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(859, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyRUWEImageToolStripMenuItem,
				(ToolStripItem)copyRUWEEvents14ToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(81, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Name("copyRUWEImageToolStripMenuItem");
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Text("Copy RUWE image");
			((ToolStripItem)copyRUWEImageToolStripMenuItem).set_Visible(false);
			((ToolStripItem)copyRUWEImageToolStripMenuItem).add_Click((EventHandler)copyRUWEImageToolStripMenuItem_Click);
			((ToolStripItem)copyRUWEEvents14ToolStripMenuItem).set_Name("copyRUWEEvents14ToolStripMenuItem");
			((ToolStripItem)copyRUWEEvents14ToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyRUWEEvents14ToolStripMenuItem).set_Text("RUWE >=1.4  Copy list of events");
			((ToolStripItem)copyRUWEEvents14ToolStripMenuItem).add_Click((EventHandler)copyRUWEEvents14ToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(243, 22));
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
			((Control)txtStatisticsList).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStatisticsList).set_Location(new Point(4, 227));
			((TextBoxBase)txtStatisticsList).set_Multiline(true);
			((Control)txtStatisticsList).set_Name("txtStatisticsList");
			txtStatisticsList.set_ScrollBars((ScrollBars)2);
			((Control)txtStatisticsList).set_Size(new Size(849, 140));
			((Control)txtStatisticsList).set_TabIndex(5);
			((Control)cmdFindMultipleEventStars).set_BackColor(Color.PowderBlue);
			((ButtonBase)cmdFindMultipleEventStars).get_FlatAppearance().set_BorderColor(Color.Black);
			((Control)cmdFindMultipleEventStars).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindMultipleEventStars).set_Location(new Point(338, 123));
			((Control)cmdFindMultipleEventStars).set_Name("cmdFindMultipleEventStars");
			((Control)cmdFindMultipleEventStars).set_Size(new Size(179, 64));
			((Control)cmdFindMultipleEventStars).set_TabIndex(6);
			((Control)cmdFindMultipleEventStars).set_Text("* Stars observed in more\r\n      than one occultation\r\n* Star magnitude statistics");
			((ButtonBase)cmdFindMultipleEventStars).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdFindMultipleEventStars).set_UseVisualStyleBackColor(false);
			((Control)cmdFindMultipleEventStars).add_Click((EventHandler)cmdFindMultipleEventStars_Click);
			((Control)panel1).set_BackColor(Color.OldLace);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)textBox1);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)txtDs);
			((Control)panel1).get_Controls().Add((Control)(object)txtDm);
			((Control)panel1).get_Controls().Add((Control)(object)txtS);
			((Control)panel1).get_Controls().Add((Control)(object)cmdFindEvents);
			((Control)panel1).get_Controls().Add((Control)(object)cmdGetCoords);
			((Control)panel1).get_Controls().Add((Control)(object)label21);
			((Control)panel1).get_Controls().Add((Control)(object)txtDd);
			((Control)panel1).get_Controls().Add((Control)(object)label18);
			((Control)panel1).get_Controls().Add((Control)(object)txtM);
			((Control)panel1).get_Controls().Add((Control)(object)txtH);
			((Control)panel1).get_Controls().Add((Control)(object)label22);
			((Control)panel1).get_Controls().Add((Control)(object)panelURAT1);
			((Control)panel1).get_Controls().Add((Control)(object)label20);
			((Control)panel1).get_Controls().Add((Control)(object)cmbCatalogue);
			((Control)panel1).get_Controls().Add((Control)(object)label23);
			((Control)panel1).get_Controls().Add((Control)(object)panelUCAC4);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)label19);
			((Control)panel1).get_Controls().Add((Control)(object)panelNOMAD);
			((Control)panel1).get_Controls().Add((Control)(object)panelB1);
			((Control)panel1).get_Controls().Add((Control)(object)panelHip);
			((Control)panel1).get_Controls().Add((Control)(object)panelUCAC3);
			((Control)panel1).get_Controls().Add((Control)(object)panelUCAC);
			((Control)panel1).get_Controls().Add((Control)(object)panelTycho2);
			((Control)panel1).set_Location(new Point(617, 25));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(236, 196));
			((Control)panel1).set_TabIndex(52);
			((Control)textBox1).set_BackColor(SystemColors.ButtonFace);
			((TextBoxBase)textBox1).set_BorderStyle((BorderStyle)0);
			((Control)textBox1).set_ForeColor(SystemColors.InfoText);
			((Control)textBox1).set_Location(new Point(28, 0));
			((Control)textBox1).set_Name("textBox1");
			((Control)textBox1).set_Size(new Size(176, 13));
			((Control)textBox1).set_TabIndex(64);
			((Control)textBox1).set_Text("Identify  past occultations of a star.");
			((Control)cmdRegionalStats).set_BackColor(Color.Thistle);
			((Control)cmdRegionalStats).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdRegionalStats).set_Location(new Point(308, 11));
			((Control)cmdRegionalStats).set_Margin(new Padding(5));
			((Control)cmdRegionalStats).set_Name("cmdRegionalStats");
			((Control)cmdRegionalStats).set_Size(new Size(105, 38));
			((Control)cmdRegionalStats).set_TabIndex(55);
			((Control)cmdRegionalStats).set_Text("Statistics");
			((ButtonBase)cmdRegionalStats).set_UseVisualStyleBackColor(false);
			((Control)cmdRegionalStats).add_Click((EventHandler)cmdRegionalStats_Click);
			((Control)updnYr1).set_Location(new Point(19, 40));
			updnYr1.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYr1.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYr1).set_Name("updnYr1");
			((Control)updnYr1).set_Size(new Size(46, 20));
			((Control)updnYr1).set_TabIndex(56);
			updnYr1.set_Value(new decimal(new int[4] { 2020, 0, 0, 0 }));
			updnYr1.add_ValueChanged((EventHandler)updnYr1_ValueChanged);
			((Control)updnYr2).set_Location(new Point(19, 77));
			updnYr2.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYr2.set_Minimum(new decimal(new int[4] { 1970, 0, 0, 0 }));
			((Control)updnYr2).set_Name("updnYr2");
			((Control)updnYr2).set_Size(new Size(46, 20));
			((Control)updnYr2).set_TabIndex(57);
			updnYr2.set_Value(new decimal(new int[4] { 2060, 0, 0, 0 }));
			updnYr2.add_ValueChanged((EventHandler)updnYr2_ValueChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_BackColor(Color.Linen);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(4, 20));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(77, 15));
			((Control)label6).set_TabIndex(58);
			((Control)label6).set_Text("Year range");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(51, 61));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(16, 13));
			((Control)label7).set_TabIndex(59);
			((Control)label7).set_Text("to");
			((Control)cmdAllObservers).set_BackColor(Color.Gold);
			((Control)cmdAllObservers).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAllObservers).set_Location(new Point(90, 11));
			((Control)cmdAllObservers).set_Name("cmdAllObservers");
			((Control)cmdAllObservers).set_Size(new Size(164, 44));
			((Control)cmdAllObservers).set_TabIndex(61);
			((Control)cmdAllObservers).set_Text("List ALL observers \r\nfor the Year range");
			((ButtonBase)cmdAllObservers).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdAllObservers).set_UseVisualStyleBackColor(false);
			((Control)cmdAllObservers).add_Click((EventHandler)cmdAllObservers_Click);
			chkByName.set_AutoCheck(false);
			((Control)chkByName).set_AutoSize(true);
			((Control)chkByName).set_BackColor(Color.LightYellow);
			chkByName.set_Checked(true);
			chkByName.set_CheckState((CheckState)1);
			((Control)chkByName).set_Location(new Point(116, 56));
			((Control)chkByName).set_Name("chkByName");
			((Control)chkByName).set_Size(new Size(68, 17));
			((Control)chkByName).set_TabIndex(62);
			((Control)chkByName).set_Text("by Name");
			((ButtonBase)chkByName).set_UseVisualStyleBackColor(false);
			((Control)chkByName).add_MouseClick(new MouseEventHandler(chkByName_MouseClick));
			chkByNumber.set_AutoCheck(false);
			((Control)chkByNumber).set_AutoSize(true);
			((Control)chkByNumber).set_BackColor(Color.LightYellow);
			((Control)chkByNumber).set_Location(new Point(116, 73));
			((Control)chkByNumber).set_Name("chkByNumber");
			((Control)chkByNumber).set_Size(new Size(112, 17));
			((Control)chkByNumber).set_TabIndex(63);
			((Control)chkByNumber).set_Text("by # Observations");
			((ButtonBase)chkByNumber).set_UseVisualStyleBackColor(false);
			((Control)chkByNumber).add_MouseClick(new MouseEventHandler(chkByNumber_MouseClick));
			((Control)panel2).set_BackColor(Color.Ivory);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)updnDiameter);
			((Control)panel2).get_Controls().Add((Control)(object)label9);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSmallAsteroids);
			((Control)panel2).get_Controls().Add((Control)(object)updnDrop);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSmallMagDrops);
			((Control)panel2).get_Controls().Add((Control)(object)chkSortMagListByNumber);
			((Control)panel2).get_Controls().Add((Control)(object)panel3);
			((Control)panel2).get_Controls().Add((Control)(object)cmdListAsteroids);
			((Control)panel2).get_Controls().Add((Control)(object)cmdFindMultipleEventStars);
			((Control)panel2).get_Controls().Add((Control)(object)label8);
			((Control)panel2).set_Location(new Point(10, 25));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(601, 196));
			((Control)panel2).set_TabIndex(64);
			updnDiameter.set_DecimalPlaces(1);
			updnDiameter.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnDiameter).set_Location(new Point(153, 172));
			updnDiameter.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnDiameter.set_Minimum(new decimal(new int[4] { 4, 0, 0, 65536 }));
			((Control)updnDiameter).set_Name("updnDiameter");
			((Control)updnDiameter).set_Size(new Size(36, 20));
			((Control)updnDiameter).set_TabIndex(71);
			updnDiameter.set_Value(new decimal(new int[4] { 3, 0, 0, 0 }));
			updnDiameter.add_ValueChanged((EventHandler)updnDiameter_ValueChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(115, 172));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(39, 17));
			((Control)label9).set_TabIndex(72);
			((Control)label9).set_Text("dia <");
			((Control)cmdSmallAsteroids).set_BackColor(Color.Yellow);
			((Control)cmdSmallAsteroids).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSmallAsteroids).set_ForeColor(SystemColors.ControlText);
			((Control)cmdSmallAsteroids).set_Location(new Point(111, 126));
			((Control)cmdSmallAsteroids).set_Name("cmdSmallAsteroids");
			((Control)cmdSmallAsteroids).set_Size(new Size(81, 42));
			((Control)cmdSmallAsteroids).set_TabIndex(70);
			((Control)cmdSmallAsteroids).set_Text("Small Dia\r\nevents");
			((ButtonBase)cmdSmallAsteroids).set_UseVisualStyleBackColor(false);
			((Control)cmdSmallAsteroids).add_Click((EventHandler)cmdSmallAsteroids_Click);
			updnDrop.set_DecimalPlaces(2);
			updnDrop.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnDrop).set_Location(new Point(53, 172));
			updnDrop.set_Maximum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			updnDrop.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnDrop).set_Name("updnDrop");
			((Control)updnDrop).set_Size(new Size(43, 20));
			((Control)updnDrop).set_TabIndex(68);
			updnDrop.set_Value(new decimal(new int[4] { 15, 0, 0, 131072 }));
			updnDrop.add_ValueChanged((EventHandler)updnDrop_ValueChanged);
			((Control)cmdSmallMagDrops).set_BackColor(Color.MistyRose);
			((Control)cmdSmallMagDrops).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSmallMagDrops).set_Location(new Point(14, 126));
			((Control)cmdSmallMagDrops).set_Name("cmdSmallMagDrops");
			((Control)cmdSmallMagDrops).set_Size(new Size(85, 42));
			((Control)cmdSmallMagDrops).set_TabIndex(67);
			((Control)cmdSmallMagDrops).set_Text("Small mag drops");
			((ButtonBase)cmdSmallMagDrops).set_UseVisualStyleBackColor(false);
			((Control)cmdSmallMagDrops).add_Click((EventHandler)cmdSmallMagDrops_Click);
			((Control)chkSortMagListByNumber).set_AutoSize(true);
			chkSortMagListByNumber.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)chkSortMagListByNumber).set_Location(new Point(520, 127));
			((Control)chkSortMagListByNumber).set_Name("chkSortMagListByNumber");
			((Control)chkSortMagListByNumber).set_Size(new Size(77, 57));
			((Control)chkSortMagListByNumber).set_TabIndex(66);
			((Control)chkSortMagListByNumber).set_Text("Sort bright\r\nstar events by\r\nstar number");
			((ButtonBase)chkSortMagListByNumber).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkSortMagListByNumber).set_UseVisualStyleBackColor(true);
			((Control)panel3).set_BackColor(Color.LightYellow);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)cmdRUWEstats);
			((Control)panel3).get_Controls().Add((Control)(object)cmdSlowEvents);
			((Control)panel3).get_Controls().Add((Control)(object)chkSaveIdentifiers);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label4);
			((Control)panel3).get_Controls().Add((Control)(object)chkByNumber);
			((Control)panel3).get_Controls().Add((Control)(object)chkByName);
			((Control)panel3).get_Controls().Add((Control)(object)label7);
			((Control)panel3).get_Controls().Add((Control)(object)label6);
			((Control)panel3).get_Controls().Add((Control)(object)updnYr2);
			((Control)panel3).get_Controls().Add((Control)(object)updnYr1);
			((Control)panel3).get_Controls().Add((Control)(object)cmdRegionalStats);
			((Control)panel3).get_Controls().Add((Control)(object)cmdAllObservers);
			((Control)panel3).set_Location(new Point(-1, -1));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(601, 118));
			((Control)panel3).set_TabIndex(65);
			((Control)cmdRUWEstats).set_BackColor(Color.SpringGreen);
			((Control)cmdRUWEstats).set_Location(new Point(468, 62));
			((Control)cmdRUWEstats).set_Name("cmdRUWEstats");
			((Control)cmdRUWEstats).set_Size(new Size(100, 44));
			((Control)cmdRUWEstats).set_TabIndex(69);
			((Control)cmdRUWEstats).set_Text("RUWE distribution");
			((ButtonBase)cmdRUWEstats).set_UseVisualStyleBackColor(false);
			((Control)cmdRUWEstats).add_Click((EventHandler)cmdRUWEstats_Click);
			((Control)cmdSlowEvents).set_BackColor(Color.Aqua);
			((Control)cmdSlowEvents).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSlowEvents).set_Location(new Point(467, 11));
			((Control)cmdSlowEvents).set_Name("cmdSlowEvents");
			((Control)cmdSlowEvents).set_Size(new Size(100, 44));
			((Control)cmdSlowEvents).set_TabIndex(68);
			((Control)cmdSlowEvents).set_Text("Slow && Fast events");
			((ButtonBase)cmdSlowEvents).set_UseVisualStyleBackColor(false);
			((Control)cmdSlowEvents).add_Click((EventHandler)cmdSlowEvents_Click);
			((Control)chkSaveIdentifiers).set_AutoSize(true);
			((Control)chkSaveIdentifiers).set_BackColor(Color.LightYellow);
			((Control)chkSaveIdentifiers).set_Location(new Point(296, 94));
			((Control)chkSaveIdentifiers).set_Name("chkSaveIdentifiers");
			((Control)chkSaveIdentifiers).set_Size(new Size(128, 17));
			((Control)chkSaveIdentifiers).set_TabIndex(66);
			((Control)chkSaveIdentifiers).set_Text("Save event identifiers");
			toolTip1.SetToolTip((Control)(object)chkSaveIdentifiers, "Saved in 'Generated files'");
			((ButtonBase)chkSaveIdentifiers).set_UseVisualStyleBackColor(false);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_BackColor(Color.LightYellow);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.DarkRed);
			((Control)label2).set_Location(new Point(98, 94));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(149, 13));
			((Control)label2).set_TabIndex(64);
			((Control)label2).set_Text("Only fitted events are included");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_BackColor(Color.LightYellow);
			((Control)label4).set_ForeColor(Color.DarkGreen);
			((Control)label4).set_Location(new Point(294, 50));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(133, 39));
			((Control)label4).set_TabIndex(65);
			((Control)label4).set_Text("REGIONS\r\nAustralasia, Europe, Japan\r\nNth America, Sth America");
			label4.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdListAsteroids).set_BackColor(Color.Aquamarine);
			((Control)cmdListAsteroids).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListAsteroids).set_Location(new Point(198, 123));
			((Control)cmdListAsteroids).set_Name("cmdListAsteroids");
			((Control)cmdListAsteroids).set_Size(new Size(135, 64));
			((Control)cmdListAsteroids).set_TabIndex(64);
			((Control)cmdListAsteroids).set_Text("Asteroids observed in one or more occultations");
			((ButtonBase)cmdListAsteroids).set_UseVisualStyleBackColor(false);
			((Control)cmdListAsteroids).add_Click((EventHandler)cmdListAsteroids_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(4, 172));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(49, 17));
			((Control)label8).set_TabIndex(69);
			((Control)label8).set_Text("drop <");
			picRUWE.set_BorderStyle((BorderStyle)2);
			((Control)picRUWE).set_Location(new Point(212, 241));
			((Control)picRUWE).set_Name("picRUWE");
			((Control)picRUWE).set_Size(new Size(359, 103));
			picRUWE.set_TabIndex(65);
			picRUWE.set_TabStop(false);
			((Control)picRUWE).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(859, 371));
			((Control)this).get_Controls().Add((Control)(object)picRUWE);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)txtStatisticsList);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("Statistics");
			((Control)this).set_Text("Statistics, and find occultations of a star");
			((Form)this).add_Load((EventHandler)FindOccultedStars_Load);
			((Control)this).add_Resize((EventHandler)FindOccultedStars_Resize);
			((Control)panelURAT1).ResumeLayout(false);
			((Control)panelURAT1).PerformLayout();
			((Control)panelUCAC4).ResumeLayout(false);
			((Control)panelUCAC4).PerformLayout();
			((Control)panelNOMAD).ResumeLayout(false);
			((Control)panelNOMAD).PerformLayout();
			((Control)panelUCAC3).ResumeLayout(false);
			((Control)panelUCAC3).PerformLayout();
			((Control)panelTycho2).ResumeLayout(false);
			((Control)panelTycho2).PerformLayout();
			((Control)panelUCAC).ResumeLayout(false);
			((Control)panelUCAC).PerformLayout();
			((Control)panelHip).ResumeLayout(false);
			((Control)panelHip).PerformLayout();
			((Control)panelB1).ResumeLayout(false);
			((Control)panelB1).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnYr1).EndInit();
			((ISupportInitialize)updnYr2).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)updnDiameter).EndInit();
			((ISupportInitialize)updnDrop).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)picRUWE).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
