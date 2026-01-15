using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class XZDoubles_Editor : Form
	{
		private string CurrentID = "";

		private string CurrentIDShort;

		private int CurrentXZ;

		private bool ChangingStar;

		private bool DoublesChanged;

		private bool OCCChanged;

		private const double Radian = 180.0 / Math.PI;

		private bool WDSUpdateCancel;

		private bool Initialised;

		private bool WDS_Entries_Added_To_XZDoubles;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstDoubles;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private GroupBox groupBox3;

		private TextBox txtRAH;

		private TextBox txtXZsecondary;

		private TextBox txtXZprinciple;

		private TextBox txtRAM;

		private TextBox txtDecD;

		private TextBox txtDecM;

		private TextBox txtCode;

		private TextBox txtPMDec;

		private TextBox txtPMRA;

		private TextBox txtPair;

		private TextBox txtNumber;

		private TextBox txtXZprimary;

		private Label label13;

		private Label label11;

		private Label label10;

		private Label label9;

		private Label label8;

		private Label label7;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private Label label17;

		private Label label16;

		private Label label15;

		private Label label14;

		private Label label18;

		private ComboBox cmbWDS1;

		private TextBox txtY1;

		private TextBox txtY2;

		private TextBox txtPA1;

		private TextBox txtPA2;

		private TextBox txtSep1;

		private TextBox txtSep2;

		private TextBox txtNumObs;

		private TextBox txtMag1;

		private TextBox txtMag2;

		private TextBox txtSpectrum;

		private ComboBox cmbWDS3;

		private ComboBox cmbWDS2;

		private Label label32;

		private Label label31;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private Label label26;

		private Label label25;

		private Label label24;

		private Label label23;

		private Label label22;

		private Label label21;

		private Label label20;

		private Label label19;

		private Label label36;

		private Label label35;

		private Label label34;

		private Label label33;

		private TextBox txtI;

		private TextBox txtA;

		private TextBox txtP;

		private TextBox txtPeri;

		private TextBox txtE;

		private TextBox txtT;

		private TextBox txtNode;

		private Label label37;

		private TextBox txtBDNumber;

		private TextBox txtBDZone;

		private Button cmdFind;

		private Button cmdAdd;

		private Button cmdReplace;

		private Button cmdDelete;

		private ComboBox cmbSep1_LessThan;

		private ComboBox cmbSep2_LessThan;

		private Label label12;

		private TextBox txtXZSearch;

		private Label label39;

		private Label label38;

		private Label label40;

		private TextBox txtIDSearchCode;

		private TextBox txtIDSearchNumber;

		private Button cmdFindID;

		private Panel panelDoubles;

		private Button cmdEditOCC;

		private Panel panelOCC;

		private Button cmdToDoubles;

		private ListBox lstOCC;

		private GroupBox groupBox4;

		private Label label41;

		private TextBox txtOCCBDNumber;

		private TextBox txtOCCBDZone;

		private Label label42;

		private Label label44;

		private Label label46;

		private Label label47;

		private Label label48;

		private Label label49;

		private Label label50;

		private Label label52;

		private Label label53;

		private Label label54;

		private Label label55;

		private Label label56;

		private Label label57;

		private TextBox txtOCCRAH;

		private TextBox txtOCCXZ;

		private TextBox txtOCCRAM;

		private TextBox txtOCCDecD;

		private TextBox txtOCCDecM;

		private TextBox txtOCCCode;

		private TextBox txtOCCRef;

		private TextBox txtOCCPair;

		private TextBox txtOCCNumber;

		private TextBox txtOCC;

		private Label label59;

		private ListBox lstNames;

		private GroupBox groupBox6;

		private GroupBox groupBox5;

		private Label label43;

		private Button cmdNameFromList;

		private TextBox txtDiscoverer;

		private Label label45;

		private TextBox txtReference;

		private Label label51;

		private Label label58;

		private Label label60;

		private ComboBox cmbD1;

		private ComboBox cmbM1;

		private NumericUpDown updnY1;

		private Label label70;

		private Label label71;

		private Label label72;

		private ComboBox cmbD4;

		private ComboBox cmbM4;

		private NumericUpDown updnY4;

		private Label label67;

		private Label label68;

		private Label label69;

		private ComboBox cmbD3;

		private ComboBox cmbM3;

		private NumericUpDown updnY3;

		private Label label64;

		private Label label65;

		private Label label66;

		private ComboBox cmbD5;

		private ComboBox cmbM5;

		private NumericUpDown updnY5;

		private Label label61;

		private Label label62;

		private Label label63;

		private ComboBox cmbD2;

		private ComboBox cmbM2;

		private NumericUpDown updnY2;

		private Label label76;

		private Label label75;

		private Label label74;

		private Label label73;

		private GroupBox groupBox7;

		private TextBox txtXZinit;

		private TextBox txtSAOinit;

		private TextBox txtZCinit;

		private Label label79;

		private Label label78;

		private Label label77;

		private Button cmdOCCinit;

		private Button cmdReplaceOCC;

		private Button cmdAddOCC;

		private Label lblCode;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkMeanPos;

		private ComboBox cmbDoubleStatus;

		private Label label80;

		private GroupBox groupBox8;

		private Button cmdCancel;

		private Button cmdWDSUpdateALL;

		private Button cmdRetrieveWDS;

		private Button cmdDoublesSearchInterferometer;

		private Button button1;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem autosearchWDSInterferometricToolStripMenuItem;

		private Button cmdCompareAllOCCwithIF;

		private ComboBox cmbLightCurve;

		private Label label81;

		private Button cmdSortXZDoubles;

		private Button cmdFindWDSMissingFromXZDoubles;

		private Button cmdGoToNextAdded;

		private Button cmdAddToOCC;

		private ToolStripMenuItem oCCStarMaintenanceToolStripMenuItem;

		private ToolStripMenuItem addEditOCCStarsToolStripMenuItem;

		private ToolStripMenuItem updateOCCStarEntriesInXZDoublesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem addNewOCCStarsToXZDoublesFromFileToolStripMenuItem;

		private ToolStripMenuItem createFileOfOCCStarsInXZDoublesToolStripMenuItem;

		public XZDoubles_Editor()
		{
			InitializeComponent();
		}

		private void XZDoubles_Editor_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			Panel obj = panelOCC;
			int left;
			((Control)panelDoubles).set_Left(left = 4);
			((Control)obj).set_Left(left);
			Panel obj2 = panelOCC;
			((Control)panelDoubles).set_Top(left = 27);
			((Control)obj2).set_Top(left);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)panelOCC).set_Visible(false);
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save XZDoubles");
			((Control)lstNames).set_Top(((Control)lstOCC).get_Top());
			((Control)lstNames).set_Visible(false);
			EditXZ_Forms.ReadDoubleStarFile();
			ComboBox obj3 = cmbSep1_LessThan;
			ComboBox obj4 = cmbSep2_LessThan;
			ComboBox obj5 = cmbWDS1;
			ComboBox obj6 = cmbWDS2;
			int num;
			((ListControl)cmbWDS3).set_SelectedIndex(num = 0);
			int num2;
			((ListControl)obj6).set_SelectedIndex(num2 = num);
			int num3;
			((ListControl)obj5).set_SelectedIndex(num3 = num2);
			((ListControl)obj4).set_SelectedIndex(left = num3);
			((ListControl)obj3).set_SelectedIndex(left);
			ComboBox obj7 = cmbDoubleStatus;
			((ListControl)cmbLightCurve).set_SelectedIndex(left = 0);
			((ListControl)obj7).set_SelectedIndex(left);
			EditXZ_Forms.ReadOCCStarFile();
			PopulateOCCList(0);
			ComboBox obj8 = cmbM1;
			ComboBox obj9 = cmbM2;
			ComboBox obj10 = cmbM3;
			ComboBox obj11 = cmbM4;
			((ListControl)cmbM5).set_SelectedIndex(num = 0);
			((ListControl)obj11).set_SelectedIndex(num2 = num);
			((ListControl)obj10).set_SelectedIndex(num3 = num2);
			((ListControl)obj9).set_SelectedIndex(left = num3);
			((ListControl)obj8).set_SelectedIndex(left);
			ComboBox obj12 = cmbD1;
			ComboBox obj13 = cmbD2;
			ComboBox obj14 = cmbD3;
			ComboBox obj15 = cmbD4;
			((ListControl)cmbD5).set_SelectedIndex(num = 0);
			((ListControl)obj15).set_SelectedIndex(num2 = num);
			((ListControl)obj14).set_SelectedIndex(num3 = num2);
			((ListControl)obj13).set_SelectedIndex(left = num3);
			((ListControl)obj12).set_SelectedIndex(left);
			((Control)cmdAddOCC).set_Enabled(false);
			((Control)cmdReplaceOCC).set_Enabled(true);
			Initialised = true;
			PopulateDoublesList(0);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)panelDoubles).get_Visible())
			{
				EditXZ_Forms.ReadDoubleStarFile();
				PopulateDoublesList(0);
			}
			else
			{
				EditXZ_Forms.ReadOCCStarFile();
				PopulateOCCList(0);
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)panelDoubles).get_Visible())
			{
				EditXZ_Forms.WriteDoubleStarFile();
				DoublesChanged = false;
			}
			else
			{
				EditXZ_Forms.WriteOCCStarFile();
				OCCChanged = false;
			}
		}

		private void Save_and_ReRead()
		{
			EditXZ_Forms.WriteDoubleStarFile();
			EditXZ_Forms.ReadDoubleStarFile();
			PopulateDoublesList(0);
		}

		private void XZDoubles_Editor_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0023: Invalid comparison between Unknown and I4
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_0035: Invalid comparison between Unknown and I4
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			//IL_0062: Invalid comparison between Unknown and I4
			//IL_0071: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Invalid comparison between Unknown and I4
			if (DoublesChanged)
			{
				DialogResult val = MessageBox.Show("You are closing the editor.\r\n\r\nChanges to XZDoubles have not been saved.\r\n\r\nDo you want to Save the changes now?", "Unsaved changes to XZDoubles", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				if ((int)val == 6)
				{
					EditXZ_Forms.WriteDoubleStarFile();
					DoublesChanged = false;
				}
				else if ((int)val == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
					return;
				}
			}
			if (OCCChanged)
			{
				DialogResult val2 = MessageBox.Show("You are closing the editor.\r\n\r\nChanges to OCC Double Discoveries have not been saved.\r\n\r\nDo you want to Save the changes now?", "Check unsaved changes", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				if ((int)val2 == 6)
				{
					EditXZ_Forms.WriteOCCStarFile();
					OCCChanged = false;
				}
				else if ((int)val2 == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
				}
			}
		}

		private void XZDoubles_Editor_FormClosed(object sender, FormClosedEventArgs e)
		{
			Interferometric_Plus_WDS.CloseInterferometerDisplay();
			Interferometric_Plus_WDS.CloseIFMismatches();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"XZDoubles Editor");
		}

		internal void PopulateDoublesList(int SelectedLine)
		{
			lstDoubles.BeginUpdate();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			lstDoubles.get_Items().Clear();
			for (int i = 0; i < EditXZ_Forms.DoubleList.Count; i++)
			{
				lstDoubles.get_Items().Add((object)EditXZ_Forms.DoubleList[i].ToString());
			}
			if ((SelectedLine >= 0) & (SelectedLine < lstDoubles.get_Items().get_Count()))
			{
				((ListControl)lstDoubles).set_SelectedIndex(SelectedLine);
			}
			lstDoubles.EndUpdate();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void lstDoubles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!Initialised)
			{
				return;
			}
			DecodeLine(((ListControl)lstDoubles).get_SelectedIndex());
			if (autosearchWDSInterferometricToolStripMenuItem.get_Checked())
			{
				if (((Control)panelDoubles).get_Visible())
				{
					cmdDoublesSearchInterferometer_Click(sender, e);
				}
				else
				{
					button1_Click(sender, e);
				}
			}
		}

		internal void DecodeLine(int LineNo)
		{
			if (LineNo < 0)
			{
				LineNo = 0;
			}
			if (lstDoubles.get_Items().get_Count() < 1)
			{
				return;
			}
			if (EditXZ_Forms.DoubleList[LineNo].XZPrimary > 0)
			{
				((Control)txtXZprimary).set_Text(EditXZ_Forms.DoubleList[LineNo].XZPrimary.ToString());
			}
			else
			{
				((Control)txtXZprimary).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].XZSecondary > 0)
			{
				((Control)txtXZsecondary).set_Text(EditXZ_Forms.DoubleList[LineNo].XZSecondary.ToString());
			}
			else
			{
				((Control)txtXZsecondary).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].XZPrinciple > 0)
			{
				((Control)txtXZprinciple).set_Text(EditXZ_Forms.DoubleList[LineNo].XZPrinciple.ToString());
			}
			else
			{
				((Control)txtXZprinciple).set_Text("");
			}
			chkMeanPos.set_Checked(EditXZ_Forms.DoubleList[LineNo].MeanPosn);
			((Control)txtRAH).set_Text(EditXZ_Forms.DoubleList[LineNo].RAH.ToString());
			((Control)txtRAM).set_Text(EditXZ_Forms.DoubleList[LineNo].RAM.ToString().PadLeft(2, '0').PadLeft(3)
				.Insert(2, "."));
			if (EditXZ_Forms.DoubleList[LineNo].DecSign)
			{
				((Control)txtDecD).set_Text(EditXZ_Forms.DoubleList[LineNo].DecD.ToString());
			}
			else
			{
				((Control)txtDecD).set_Text("-" + EditXZ_Forms.DoubleList[LineNo].DecD);
			}
			((Control)txtDecM).set_Text(EditXZ_Forms.DoubleList[LineNo].DecM.ToString());
			((Control)txtCode).set_Text(EditXZ_Forms.DoubleList[LineNo].IDCode);
			((Control)txtNumber).set_Text(EditXZ_Forms.DoubleList[LineNo].IDNumber);
			((Control)txtPair).set_Text(EditXZ_Forms.DoubleList[LineNo].IDPair);
			if (EditXZ_Forms.DoubleList[LineNo].PMRA > -999)
			{
				((Control)txtPMRA).set_Text(EditXZ_Forms.DoubleList[LineNo].PMRA.ToString());
			}
			else
			{
				((Control)txtPMRA).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].PMDec > -999)
			{
				((Control)txtPMDec).set_Text(EditXZ_Forms.DoubleList[LineNo].PMDec.ToString());
			}
			else
			{
				((Control)txtPMDec).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].BDZone > -90)
			{
				if (EditXZ_Forms.DoubleList[LineNo].BDSign)
				{
					((Control)txtBDZone).set_Text(EditXZ_Forms.DoubleList[LineNo].BDZone.ToString());
				}
				else
				{
					((Control)txtBDZone).set_Text("-" + EditXZ_Forms.DoubleList[LineNo].BDZone);
				}
				((Control)txtBDNumber).set_Text(EditXZ_Forms.DoubleList[LineNo].BDNumber.ToString());
			}
			else
			{
				TextBox obj = txtBDZone;
				string text;
				((Control)txtBDNumber).set_Text(text = "");
				((Control)obj).set_Text(text);
			}
			if (EditXZ_Forms.DoubleList[LineNo].Y1 > 0)
			{
				((Control)txtY1).set_Text(EditXZ_Forms.DoubleList[LineNo].Y1.ToString());
			}
			else
			{
				((Control)txtY1).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Y2 > 0)
			{
				((Control)txtY2).set_Text(EditXZ_Forms.DoubleList[LineNo].Y2.ToString());
			}
			else
			{
				((Control)txtY2).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].PA1 > 0.0)
			{
				((Control)txtPA1).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].PA1, 1, EditXZ_Forms.DoubleList[LineNo].PA1_Places));
			}
			else
			{
				((Control)txtPA1).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].PA2 > 0.0)
			{
				((Control)txtPA2).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].PA2, 1, EditXZ_Forms.DoubleList[LineNo].PA2_Places));
			}
			else
			{
				((Control)txtPA2).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Sep1 > 0.0)
			{
				((Control)txtSep1).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Sep1, 1, EditXZ_Forms.DoubleList[LineNo].Sep1_Places));
			}
			else
			{
				((Control)txtSep1).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Sep1_LessThan)
			{
				((ListControl)cmbSep1_LessThan).set_SelectedIndex(1);
			}
			else
			{
				((ListControl)cmbSep1_LessThan).set_SelectedIndex(0);
			}
			if (EditXZ_Forms.DoubleList[LineNo].Sep2 > 0.0)
			{
				((Control)txtSep2).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Sep2, 1, EditXZ_Forms.DoubleList[LineNo].Sep2_Places));
			}
			else
			{
				((Control)txtSep2).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Sep2_LessThan)
			{
				((ListControl)cmbSep2_LessThan).set_SelectedIndex(1);
			}
			else
			{
				((ListControl)cmbSep2_LessThan).set_SelectedIndex(0);
			}
			if (EditXZ_Forms.DoubleList[LineNo].NumOfObs > 0)
			{
				((Control)txtNumObs).set_Text(EditXZ_Forms.DoubleList[LineNo].NumOfObs.ToString());
			}
			else
			{
				((Control)txtNumObs).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Mag1 > 0.0)
			{
				((Control)txtMag1).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Mag1, 1, EditXZ_Forms.DoubleList[LineNo].Mag1_Places));
			}
			else
			{
				((Control)txtMag1).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Mag2 > 0.0)
			{
				((Control)txtMag2).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Mag2, 1, EditXZ_Forms.DoubleList[LineNo].Mag2_Places));
			}
			else
			{
				((Control)txtMag2).set_Text("");
			}
			((Control)txtSpectrum).set_Text(EditXZ_Forms.DoubleList[LineNo].Spectrum.Trim());
			ComboBox obj2 = cmbWDS1;
			ComboBox obj3 = cmbWDS2;
			int num;
			((ListControl)cmbWDS3).set_SelectedIndex(num = 0);
			int selectedIndex;
			((ListControl)obj3).set_SelectedIndex(selectedIndex = num);
			((ListControl)obj2).set_SelectedIndex(selectedIndex);
			int num2 = " DNOPQRaprs6XZ".IndexOf(EditXZ_Forms.DoubleList[LineNo].WDSNote1.ToString());
			if (num2 >= 0)
			{
				((ListControl)cmbWDS1).set_SelectedIndex(num2);
			}
			num2 = " DNOPQRaprs6XZ".IndexOf(EditXZ_Forms.DoubleList[LineNo].WDSNote2.ToString());
			if (num2 >= 0)
			{
				((ListControl)cmbWDS2).set_SelectedIndex(num2);
			}
			num2 = " DNOPQRaprs6XZ".IndexOf(EditXZ_Forms.DoubleList[LineNo].WDSNote3.ToString());
			if (num2 >= 0)
			{
				((ListControl)cmbWDS3).set_SelectedIndex(num2);
			}
			if (EditXZ_Forms.DoubleList[LineNo].Period > 0.0)
			{
				((Control)txtP).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Period, 1, EditXZ_Forms.DoubleList[LineNo].Period_Places));
			}
			else
			{
				((Control)txtP).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Axis > -0.1)
			{
				((Control)txtA).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Axis, 1, EditXZ_Forms.DoubleList[LineNo].Axis_Places));
			}
			else
			{
				((Control)txtA).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Inclination >= 0.0)
			{
				((Control)txtI).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Inclination, 1, EditXZ_Forms.DoubleList[LineNo].Inclination_Places));
			}
			else
			{
				((Control)txtI).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Node >= 0.0)
			{
				((Control)txtNode).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Node, 1, EditXZ_Forms.DoubleList[LineNo].Node_Places));
			}
			else
			{
				((Control)txtNode).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].T > 0.0)
			{
				((Control)txtT).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].T, 1, EditXZ_Forms.DoubleList[LineNo].T_Places));
			}
			else
			{
				((Control)txtT).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].E >= 0.0)
			{
				((Control)txtE).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].E, 1, EditXZ_Forms.DoubleList[LineNo].E_Places));
			}
			else
			{
				((Control)txtE).set_Text("");
			}
			if (EditXZ_Forms.DoubleList[LineNo].Periastron >= 0.0)
			{
				((Control)txtPeri).set_Text(Utilities.FormatNumber(EditXZ_Forms.DoubleList[LineNo].Periastron, 1, EditXZ_Forms.DoubleList[LineNo].Periastron_Places));
			}
			else
			{
				((Control)txtPeri).set_Text("");
			}
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstDoubles).get_SelectedIndex();
			if ((int)MessageBox.Show("Do you want to replace\r\n\r\n" + lstDoubles.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 98), "Confirm Replace", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				EncodeLine(selectedIndex);
				PopulateDoublesList(selectedIndex);
				DoublesChanged = true;
				((Control)EditXZ_Forms.DoubleEditor.lstDoubles).Focus();
			}
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			XZDoubles item = new XZDoubles();
			EditXZ_Forms.DoubleList.Add(item);
			EncodeLine(EditXZ_Forms.DoubleList.Count - 1);
			if (EditXZ_Forms.DoubleList[EditXZ_Forms.DoubleList.Count - 1].IDCode.ToUpper() == "OCC")
			{
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\XZDoubles_OCC_additions.dat", append: true);
				streamWriter.WriteLine(EditXZ_Forms.DoubleList[EditXZ_Forms.DoubleList.Count - 1].SaveString());
				MessageBox.Show("OCC star has ALSO been added to the XZDoubles_OCC_additions.dat file");
			}
			EditXZ_Forms.DoubleList.Sort();
			int selectedLine = EditXZ_Forms.DoubleList.FindIndex(MatchesID);
			PopulateDoublesList(selectedLine);
			DoublesChanged = true;
		}

		private bool MatchesID(XZDoubles Doubles)
		{
			return Doubles.IDCode + Doubles.IDNumber + Doubles.IDPair == CurrentID;
		}

		internal void EncodeLine(int Line)
		{
			int result = 0;
			int result2 = 0;
			if (!int.TryParse(((Control)txtXZprimary).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 244437 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].XZPrimary = result;
			if (!int.TryParse(((Control)txtXZsecondary).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 244437 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].XZSecondary = result;
			if (!int.TryParse(((Control)txtXZprinciple).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 244437 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].XZPrinciple = result;
			EditXZ_Forms.DoubleList[Line].MeanPosn = chkMeanPos.get_Checked();
			if (!int.TryParse(((Control)txtRAH).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 23 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].RAH = result;
			if (!int.TryParse(((Control)txtRAM).get_Text().Replace(".", ""), out result))
			{
				result = 0;
			}
			if (result > 599 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].RAM = result;
			EditXZ_Forms.DoubleList[Line].DecSign = !((Control)txtDecD).get_Text().Contains("-");
			if (!int.TryParse(((Control)txtDecD).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			if (result > 89)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].DecD = result;
			if (!int.TryParse(((Control)txtDecM).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 59 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.DoubleList[Line].DecM = result;
			EditXZ_Forms.DoubleList[Line].IDCode = ((Control)txtCode).get_Text().Trim().PadRight(3)
				.Substring(0, 3);
			if (int.TryParse(((Control)txtNumber).get_Text(), out result))
			{
				EditXZ_Forms.DoubleList[Line].IDNumber = ((Control)txtNumber).get_Text().Trim().PadLeft(4)
					.Substring(0, 4);
			}
			else
			{
				EditXZ_Forms.DoubleList[Line].IDNumber = ((Control)txtNumber).get_Text().Trim().PadRight(4)
					.Substring(0, 4);
			}
			EditXZ_Forms.DoubleList[Line].IDPair = ((Control)txtPair).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			if (!int.TryParse(((Control)txtPMRA).get_Text(), out result))
			{
				result = -999;
			}
			if (result < -999 || result > 999)
			{
				result = -999;
			}
			EditXZ_Forms.DoubleList[Line].PMRA = result;
			if (!int.TryParse(((Control)txtPMDec).get_Text(), out result))
			{
				result = -999;
			}
			if (result < -999 || result > 999)
			{
				result = -999;
			}
			EditXZ_Forms.DoubleList[Line].PMDec = result;
			if (!int.TryParse(((Control)txtBDZone).get_Text().Replace("-", ""), out result))
			{
				result = -99;
			}
			if (!int.TryParse(((Control)txtBDNumber).get_Text(), out result2))
			{
				result2 = -1;
			}
			if (result > -90 && result2 > 0 && result2 < 99999)
			{
				EditXZ_Forms.DoubleList[Line].BDZone = result;
				EditXZ_Forms.DoubleList[Line].BDNumber = result2;
				EditXZ_Forms.DoubleList[Line].BDSign = !((Control)txtBDZone).get_Text().Contains("-");
			}
			else
			{
				EditXZ_Forms.DoubleList[Line].BDZone = -99;
				EditXZ_Forms.DoubleList[Line].BDNumber = -1;
				EditXZ_Forms.DoubleList[Line].BDSign = true;
			}
			if (!int.TryParse(((Control)txtY1).get_Text(), out result))
			{
				result = -1;
			}
			if (result < -1 || result > 2100)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].Y1 = result;
			if (!int.TryParse(((Control)txtY2).get_Text(), out result))
			{
				result = -1;
			}
			if (result < -1 || result > 2100)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].Y2 = result;
			if (!double.TryParse(((Control)txtPA1).get_Text(), out var result3))
			{
				result3 = -1.0;
			}
			if (result3 >= 360.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].PA1 = result3;
			EditXZ_Forms.DoubleList[Line].PA1_Places = Utilities.DecimalPlaces(((Control)txtPA1).get_Text());
			if (!double.TryParse(((Control)txtPA2).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 >= 360.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].PA2 = result3;
			EditXZ_Forms.DoubleList[Line].PA2_Places = Utilities.DecimalPlaces(((Control)txtPA2).get_Text());
			if (!double.TryParse(((Control)txtSep1).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 900.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Sep1 = result3;
			EditXZ_Forms.DoubleList[Line].Sep1_Places = Utilities.DecimalPlaces(((Control)txtSep1).get_Text());
			EditXZ_Forms.DoubleList[Line].Sep1_LessThan = ((ListControl)cmbSep1_LessThan).get_SelectedIndex() == 1;
			if (!double.TryParse(((Control)txtSep2).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 900.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Sep2 = result3;
			EditXZ_Forms.DoubleList[Line].Sep2_Places = Utilities.DecimalPlaces(((Control)txtSep2).get_Text());
			EditXZ_Forms.DoubleList[Line].Sep2_LessThan = ((ListControl)cmbSep2_LessThan).get_SelectedIndex() == 1;
			if (!int.TryParse(((Control)txtNumObs).get_Text(), out result))
			{
				result = -1;
			}
			if (result > 99)
			{
				result = 99;
			}
			if (result < -1)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].NumOfObs = result;
			if (!double.TryParse(((Control)txtMag1).get_Text(), out result3))
			{
				result3 = -10.0;
			}
			if (result3 > 20.0)
			{
				result3 = -10.0;
			}
			if (result3 < -10.0)
			{
				result3 = -10.0;
			}
			EditXZ_Forms.DoubleList[Line].Mag1 = result3;
			EditXZ_Forms.DoubleList[Line].Mag1_Places = Utilities.DecimalPlaces(((Control)txtMag1).get_Text());
			if (!double.TryParse(((Control)txtMag2).get_Text(), out result3))
			{
				result3 = -10.0;
			}
			if (result3 > 20.0)
			{
				result3 = -10.0;
			}
			if (result3 < -10.0)
			{
				result3 = -10.0;
			}
			EditXZ_Forms.DoubleList[Line].Mag2 = result3;
			EditXZ_Forms.DoubleList[Line].Mag2_Places = Utilities.DecimalPlaces(((Control)txtMag2).get_Text());
			EditXZ_Forms.DoubleList[Line].Spectrum = ((Control)txtSpectrum).get_Text().Trim().PadRight(10)
				.Substring(0, 10);
			EditXZ_Forms.DoubleList[Line].WDSNote1 = cmbWDS1.get_Items().get_Item(((ListControl)cmbWDS1).get_SelectedIndex()).ToString();
			EditXZ_Forms.DoubleList[Line].WDSNote2 = cmbWDS1.get_Items().get_Item(((ListControl)cmbWDS2).get_SelectedIndex()).ToString();
			EditXZ_Forms.DoubleList[Line].WDSNote3 = cmbWDS1.get_Items().get_Item(((ListControl)cmbWDS3).get_SelectedIndex()).ToString();
			if (!double.TryParse(((Control)txtP).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 9999.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Period = result3;
			EditXZ_Forms.DoubleList[Line].Period_Places = Utilities.DecimalPlaces(((Control)txtP).get_Text());
			if (!double.TryParse(((Control)txtA).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 99.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Axis = result3;
			EditXZ_Forms.DoubleList[Line].Axis_Places = Utilities.DecimalPlaces(((Control)txtA).get_Text());
			if (!double.TryParse(((Control)txtI).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 180.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Inclination = result3;
			EditXZ_Forms.DoubleList[Line].Inclination_Places = Utilities.DecimalPlaces(((Control)txtI).get_Text());
			if (!double.TryParse(((Control)txtNode).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 360.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Node = result3;
			EditXZ_Forms.DoubleList[Line].Node_Places = Utilities.DecimalPlaces(((Control)txtNode).get_Text());
			if (!double.TryParse(((Control)txtT).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 9999.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].T = result3;
			EditXZ_Forms.DoubleList[Line].T_Places = Utilities.DecimalPlaces(((Control)txtT).get_Text());
			if (!double.TryParse(((Control)txtE).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 1.2)
			{
				result3 = -1.0;
			}
			if (result3 < 0.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].E = result3;
			EditXZ_Forms.DoubleList[Line].E_Places = Utilities.DecimalPlaces(((Control)txtE).get_Text());
			if (!double.TryParse(((Control)txtPeri).get_Text(), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 360.0)
			{
				result3 = -1.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Periastron = result3;
			EditXZ_Forms.DoubleList[Line].Periastron_Places = Utilities.DecimalPlaces(((Control)txtPeri).get_Text());
			CurrentID = EditXZ_Forms.DoubleList[Line].IDCode + EditXZ_Forms.DoubleList[Line].IDNumber + EditXZ_Forms.DoubleList[Line].IDPair;
		}

		private void cmdFind_Click(object sender, EventArgs e)
		{
			if (int.TryParse(((Control)txtXZSearch).get_Text(), out CurrentXZ) && !((CurrentXZ < 1) | (CurrentXZ > 244437)))
			{
				((ListControl)lstDoubles).set_SelectedIndex(EditXZ_Forms.DoubleList.FindIndex(MatchesXZ));
				if ((((ListControl)lstDoubles).get_SelectedIndex() < 0) & (lstDoubles.get_Items().get_Count() > 0))
				{
					((ListControl)lstDoubles).set_SelectedIndex(0);
				}
			}
		}

		private bool MatchesXZ(XZDoubles Doubles)
		{
			return (Doubles.XZPrimary == CurrentXZ) | (Doubles.XZSecondary == CurrentXZ) | (Doubles.XZPrinciple == CurrentXZ);
		}

		private void cmdFindID_Click(object sender, EventArgs e)
		{
			if (int.TryParse(((Control)txtIDSearchNumber).get_Text(), out var _))
			{
				CurrentIDShort = (((Control)txtIDSearchCode).get_Text().Trim().PadRight(3)
					.Substring(0, 3) + ((Control)txtIDSearchNumber).get_Text().Trim().PadLeft(4)
					.Substring(0, 4)).ToUpper();
			}
			else
			{
				CurrentIDShort = (((Control)txtIDSearchCode).get_Text().Trim().PadRight(3)
					.Substring(0, 3) + ((Control)txtIDSearchNumber).get_Text().Trim().PadRight(4)
					.Substring(0, 4)).ToUpper();
			}
			((ListControl)lstDoubles).set_SelectedIndex(EditXZ_Forms.DoubleList.FindIndex(MatchesIDShort));
			if ((((ListControl)lstDoubles).get_SelectedIndex() < 0) & (lstDoubles.get_Items().get_Count() > 0))
			{
				((ListControl)lstDoubles).set_SelectedIndex(0);
			}
		}

		private bool MatchesIDShort(XZDoubles Doubles)
		{
			return Doubles.IDCode.ToUpper() + Doubles.IDNumber.ToUpper() == CurrentIDShort;
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Invalid comparison between Unknown and I4
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a1: Invalid comparison between Unknown and I4
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ff: Invalid comparison between Unknown and I4
			//IL_014f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstDoubles).get_SelectedIndex();
			if ((int)MessageBox.Show("Do you want to delete\r\n\r\n" + lstDoubles.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 98), "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7 && (!EditXZ_Forms.DoubleList[selectedIndex].MeanPosn || (int)MessageBox.Show("This item has the Mean position flag set. It needs to be set in any replacement entry.\r\n\r\nDo you want to delete\r\n\r\n" + lstDoubles.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 98), "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7) && (!(EditXZ_Forms.DoubleList[selectedIndex].T > 0.0) || (int)MessageBox.Show("This item has orbital elements. They need to be set in any replacement entry.\r\n\r\nDo you want to delete\r\n\r\n" + lstDoubles.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 98), "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7) && (EditXZ_Forms.DoubleList[selectedIndex].XZSecondary <= 0 || (int)MessageBox.Show("An XZ star is listed for the secondary. It needs to be set in any replacement entry.\r\n\r\nDo you want to delete\r\n\r\n" + lstDoubles.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 98), "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7))
			{
				EditXZ_Forms.DoubleList.RemoveAt(selectedIndex);
				if (selectedIndex > 0)
				{
					PopulateDoublesList(selectedIndex - 1);
				}
				else
				{
					PopulateDoublesList(0);
				}
				DoublesChanged = true;
				((Control)EditXZ_Forms.DoubleEditor.lstDoubles).Focus();
			}
		}

		private void cmdSyncWithOCC_Click(object sender, EventArgs e)
		{
			SyncXZDoublesWithOCCstars();
		}

		private void SyncXZDoublesWithOCCstars()
		{
			for (int i = 0; i < lstDoubles.get_Items().get_Count(); i++)
			{
				if (!((EditXZ_Forms.DoubleList[i].IDCode == "OCC") | (EditXZ_Forms.DoubleList[i].IDCode == "OCc") | (EditXZ_Forms.DoubleList[i].IDCode == "OC*")))
				{
					continue;
				}
				if (!int.TryParse(EditXZ_Forms.DoubleList[i].IDNumber, out var result))
				{
					result = 0;
				}
				if (result > EditXZ_Forms.OCCList.Count)
				{
					continue;
				}
				switch (EditXZ_Forms.OCCList[result - 1].Status)
				{
				case 0:
					if (EditXZ_Forms.OCCList[result - 1].IDCode.Trim().Length > 0)
					{
						EditXZ_Forms.DoubleList[i].IDCode = EditXZ_Forms.OCCList[result - 1].IDCode;
						EditXZ_Forms.DoubleList[i].IDNumber = EditXZ_Forms.OCCList[result - 1].IDNumber;
						EditXZ_Forms.DoubleList[i].IDPair = EditXZ_Forms.OCCList[result - 1].IDPair;
					}
					break;
				case 1:
					EditXZ_Forms.DoubleList[i].IDCode = "OCc";
					break;
				case 2:
					EditXZ_Forms.DoubleList[i].IDCode = "OC*";
					break;
				}
			}
			PopulateDoublesList(((ListControl)lstDoubles).get_SelectedIndex());
			DoublesChanged = true;
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			WDSUpdateCancel = true;
		}

		private void cmdWDSUpdateALL_Click(object sender, EventArgs e)
		{
			if (UpdateUsingWDS(UpdateAll: true))
			{
				Save_and_ReRead();
			}
		}

		private bool UpdateUsingWDS(bool UpdateAll)
		{
			string text = "";
			WDSUpdateCancel = false;
			Cursor.set_Current(Cursors.get_Default());
			((Control)cmdCancel).set_Visible(true);
			bool result = false;
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Downloaded Files\\Unmatched WDS Stars.txt"))
			{
				streamWriter.WriteLine("This file lists pairs in XZDoubles.dat that were not matched\r\n to pairs in the WDS on " + DateTime.Now.ToShortDateString() + "\r\n");
				Cursor.set_Current(Cursors.get_WaitCursor());
				Access_WDS_by_Name.PrepareToAccessWDS();
				for (int i = 0; i < lstDoubles.get_Items().get_Count(); i++)
				{
					text = EditXZ_Forms.DoubleList[i].IDCode + EditXZ_Forms.DoubleList[i].IDNumber + EditXZ_Forms.DoubleList[i].IDPair;
					if (Access_WDS_by_Name.ReadWDS(text, out var Line, out var _, ReturnWDSLine: true))
					{
						if (!int.TryParse(Line.Substring(33, 4), out var result2))
						{
							result2 = 0;
						}
						if (UpdateAll | (result2 > EditXZ_Forms.DoubleList[i].NumOfObs))
						{
							UpdateDoubleEntry(i, Line);
							result = true;
						}
					}
					else
					{
						streamWriter.WriteLine(text + "  " + EditXZ_Forms.DoubleList[i].RA.ToString().PadLeft(5, '0') + EditXZ_Forms.DoubleList[i].Dec.ToString().PadLeft(5).Replace(" -", "-0")
							.Replace("  ", "+0"));
					}
					Application.DoEvents();
					if (WDSUpdateCancel)
					{
						break;
					}
				}
				Access_WDS_by_Name.EndAccessToWDS();
			}
			PopulateDoublesList(((ListControl)lstDoubles).get_SelectedIndex());
			Cursor.set_Current(Cursors.get_Default());
			((Control)cmdCancel).set_Visible(false);
			DoublesChanged = true;
			return result;
		}

		private void UpdateDoubleEntry(int Line, string WDSLine)
		{
			if (!int.TryParse(WDSLine.Substring(80, 4), out var result))
			{
				result = -999;
			}
			if (result < -999 || result > 999)
			{
				result = -999;
			}
			EditXZ_Forms.DoubleList[Line].PMRA = result;
			if (!int.TryParse(WDSLine.Substring(84, 4), out result))
			{
				result = -999;
			}
			if (result < -999 || result > 999)
			{
				result = -999;
			}
			EditXZ_Forms.DoubleList[Line].PMDec = result;
			if (!int.TryParse(WDSLine.Substring(98, 3).Replace("-", ""), out result))
			{
				result = -99;
			}
			if (!int.TryParse(WDSLine.Substring(101, 5), out var result2))
			{
				result2 = -1;
			}
			if (result > -90 && result2 > 0 && result2 < 99999)
			{
				EditXZ_Forms.DoubleList[Line].BDZone = result;
				EditXZ_Forms.DoubleList[Line].BDNumber = result2;
				EditXZ_Forms.DoubleList[Line].BDSign = !WDSLine.Substring(98, 3).Contains("-");
			}
			else
			{
				EditXZ_Forms.DoubleList[Line].BDZone = -99;
				EditXZ_Forms.DoubleList[Line].BDNumber = -1;
				EditXZ_Forms.DoubleList[Line].BDSign = true;
			}
			if (!int.TryParse(WDSLine.Substring(23, 4), out result))
			{
				result = -1;
			}
			if (result < -1 || result > 2100)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].Y1 = result;
			if (!int.TryParse(WDSLine.Substring(28, 4), out result))
			{
				result = -1;
			}
			if (result < -1 || result > 2100)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].Y2 = result;
			if (!double.TryParse(WDSLine.Substring(38, 3), out var result3))
			{
				result3 = -1.0;
			}
			if (result3 >= 360.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].PA1 = result3;
			EditXZ_Forms.DoubleList[Line].PA1_Places = Utilities.DecimalPlaces(WDSLine.Substring(38, 3));
			if (!double.TryParse(WDSLine.Substring(42, 3), out result3))
			{
				result3 = -1.0;
			}
			if (result3 >= 360.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].PA2 = result3;
			EditXZ_Forms.DoubleList[Line].PA2_Places = Utilities.DecimalPlaces(WDSLine.Substring(42, 3));
			if (!double.TryParse(WDSLine.Substring(46, 5), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 900.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Sep1 = result3;
			EditXZ_Forms.DoubleList[Line].Sep1_Places = Utilities.DecimalPlaces(WDSLine.Substring(46, 5));
			EditXZ_Forms.DoubleList[Line].Sep1_LessThan = false;
			if (!double.TryParse(WDSLine.Substring(52, 5), out result3))
			{
				result3 = -1.0;
			}
			if (result3 > 900.0)
			{
				result3 = 0.0;
			}
			if (result3 < -1.0)
			{
				result3 = -1.0;
			}
			EditXZ_Forms.DoubleList[Line].Sep2 = result3;
			EditXZ_Forms.DoubleList[Line].Sep2_Places = Utilities.DecimalPlaces(WDSLine.Substring(52, 5));
			EditXZ_Forms.DoubleList[Line].Sep2_LessThan = false;
			if (!int.TryParse(WDSLine.Substring(33, 4), out result))
			{
				result = -1;
			}
			if (result > 99)
			{
				result = 99;
			}
			if (result < -1)
			{
				result = -1;
			}
			EditXZ_Forms.DoubleList[Line].NumOfObs = result;
			if (!double.TryParse(WDSLine.Substring(58, 5), out result3))
			{
				result3 = -10.0;
			}
			if (result3 > 20.0)
			{
				result3 = -10.0;
			}
			if (result3 < -10.0)
			{
				result3 = -10.0;
			}
			EditXZ_Forms.DoubleList[Line].Mag1 = result3;
			EditXZ_Forms.DoubleList[Line].Mag1_Places = Utilities.DecimalPlaces(WDSLine.Substring(58, 5));
			if (!double.TryParse(WDSLine.Substring(64, 5), out result3))
			{
				result3 = -10.0;
			}
			if (result3 > 20.0)
			{
				result3 = -10.0;
			}
			if (result3 < -10.0)
			{
				result3 = -10.0;
			}
			EditXZ_Forms.DoubleList[Line].Mag2 = result3;
			EditXZ_Forms.DoubleList[Line].Mag2_Places = Utilities.DecimalPlaces(WDSLine.Substring(64, 5));
			EditXZ_Forms.DoubleList[Line].Spectrum = WDSLine.Substring(70, 9).PadRight(10);
		}

		private void cmdRetrieveWDS_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			int selectedIndex = ((ListControl)lstDoubles).get_SelectedIndex();
			if (Access_WDS_by_Name.ReadWDS(EditXZ_Forms.DoubleList[selectedIndex].IDCode + EditXZ_Forms.DoubleList[selectedIndex].IDNumber + EditXZ_Forms.DoubleList[selectedIndex].IDPair, out var Line, out var _, ReturnWDSLine: true))
			{
				((Control)txtY1).set_Text(Line.Substring(23, 4));
				((Control)txtY2).set_Text(Line.Substring(28, 4));
				((Control)txtNumObs).set_Text(Line.Substring(33, 4));
				((Control)txtPA1).set_Text(Line.Substring(38, 3));
				((Control)txtPA2).set_Text(Line.Substring(42, 3));
				((Control)txtSep1).set_Text(Line.Substring(46, 5));
				((Control)txtSep2).set_Text(Line.Substring(52, 5));
				((Control)txtMag1).set_Text(Line.Substring(58, 5));
				((Control)txtMag2).set_Text(Line.Substring(64, 5));
				((Control)txtSpectrum).set_Text(Line.Substring(70, 9));
				((Control)txtPMRA).set_Text(Line.Substring(80, 4));
				((Control)txtPMDec).set_Text(Line.Substring(84, 4));
				((Control)txtBDZone).set_Text(Line.Substring(98, 3));
				((Control)txtBDNumber).set_Text(Line.Substring(101, 5));
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdSortXZDoubles_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstDoubles).get_SelectedIndex();
			EditXZ_Forms.DoubleList.Sort();
			PopulateDoublesList(selectedIndex);
			DoublesChanged = true;
		}

		internal void PopulateOCCList(int SelectedLine)
		{
			lstOCC.BeginUpdate();
			lstNames.BeginUpdate();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			lstOCC.get_Items().Clear();
			for (int i = 0; i < EditXZ_Forms.OCCList.Count; i++)
			{
				lstOCC.get_Items().Add((object)EditXZ_Forms.OCCList[i].ToString());
				if (EditXZ_Forms.OCCList[i].Discoverer.Trim().Length > 0)
				{
					lstNames.get_Items().Add((object)EditXZ_Forms.OCCList[i].Discoverer.Trim());
				}
			}
			if ((SelectedLine >= 0) & (SelectedLine < lstOCC.get_Items().get_Count()))
			{
				((ListControl)lstOCC).set_SelectedIndex(SelectedLine);
			}
			for (int num = lstNames.get_Items().get_Count() - 1; num >= 1; num--)
			{
				if (lstNames.get_Items().get_Item(num).ToString() == lstNames.get_Items().get_Item(num - 1).ToString())
				{
					lstNames.get_Items().RemoveAt(num - 1);
				}
			}
			lstNames.get_Items().Add((object)" **Cancel**");
			lstOCC.EndUpdate();
			lstNames.EndUpdate();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdEditOCC_Click(object sender, EventArgs e)
		{
			EditOCCstars();
		}

		private void EditOCCstars()
		{
			((Control)panelDoubles).set_Visible(false);
			((Control)panelOCC).set_Visible(true);
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save OCC Double Discoveries");
			((ToolStripItem)oCCStarMaintenanceToolStripMenuItem).set_Enabled(false);
			if ((((Control)txtCode).get_Text() == "OCC") | (((Control)txtCode).get_Text() == "OCc") | (((Control)txtCode).get_Text() == "OC*"))
			{
				if (!int.TryParse(((Control)txtNumber).get_Text(), out var result))
				{
					result = 1;
				}
				if (result > lstOCC.get_Items().get_Count())
				{
					result = 1;
				}
				((ListControl)lstOCC).set_SelectedIndex(result - 1);
			}
		}

		private void cmdToDoubles_Click(object sender, EventArgs e)
		{
			((Control)panelDoubles).set_Visible(true);
			((Control)panelOCC).set_Visible(false);
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save XZDoubles");
			((ToolStripItem)oCCStarMaintenanceToolStripMenuItem).set_Enabled(true);
		}

		private void DecodeOCCLine(int LineNo)
		{
			if (LineNo < 0)
			{
				LineNo = 0;
			}
			if (lstOCC.get_Items().get_Count() < 1)
			{
				return;
			}
			if (EditXZ_Forms.OCCList[LineNo].OCC > 0)
			{
				((Control)txtOCC).set_Text(EditXZ_Forms.OCCList[LineNo].OCC.ToString());
			}
			else
			{
				((Control)txtOCC).set_Text("0");
			}
			if (EditXZ_Forms.OCCList[LineNo].XZ > 0)
			{
				((Control)txtOCCXZ).set_Text(EditXZ_Forms.OCCList[LineNo].XZ.ToString());
			}
			else
			{
				((Control)txtOCCXZ).set_Text("");
			}
			((Control)txtOCCRAH).set_Text(EditXZ_Forms.OCCList[LineNo].RAH.ToString());
			((Control)txtOCCRAM).set_Text(EditXZ_Forms.OCCList[LineNo].RAM.ToString().PadLeft(2, '0').PadLeft(3)
				.Insert(2, "."));
			if (EditXZ_Forms.OCCList[LineNo].DecSign)
			{
				((Control)txtOCCDecD).set_Text(EditXZ_Forms.OCCList[LineNo].DecD.ToString());
			}
			else
			{
				((Control)txtOCCDecD).set_Text("-" + EditXZ_Forms.OCCList[LineNo].DecD);
			}
			((Control)txtOCCDecM).set_Text(EditXZ_Forms.OCCList[LineNo].DecM.ToString());
			((Control)txtOCCCode).set_Text(EditXZ_Forms.OCCList[LineNo].IDCode);
			((Control)txtOCCNumber).set_Text(EditXZ_Forms.OCCList[LineNo].IDNumber);
			((Control)txtOCCPair).set_Text(EditXZ_Forms.OCCList[LineNo].IDPair);
			if (EditXZ_Forms.OCCList[LineNo].BDZone > -90)
			{
				if (EditXZ_Forms.OCCList[LineNo].BDSign)
				{
					((Control)txtOCCBDZone).set_Text(EditXZ_Forms.OCCList[LineNo].BDZone.ToString());
				}
				else
				{
					((Control)txtOCCBDZone).set_Text("-" + EditXZ_Forms.OCCList[LineNo].BDZone);
				}
				((Control)txtOCCBDNumber).set_Text(EditXZ_Forms.OCCList[LineNo].BDNumber.ToString());
			}
			else
			{
				TextBox obj = txtOCCBDZone;
				string text;
				((Control)txtOCCBDNumber).set_Text(text = "");
				((Control)obj).set_Text(text);
			}
			((Control)txtOCCRef).set_Text(EditXZ_Forms.OCCList[LineNo].dsFile);
			((Control)txtDiscoverer).set_Text(EditXZ_Forms.OCCList[LineNo].Discoverer);
			((Control)txtReference).set_Text(EditXZ_Forms.OCCList[LineNo].Reference);
			if (EditXZ_Forms.OCCList[LineNo].Y1 > 0)
			{
				updnY1.set_Value((decimal)EditXZ_Forms.OCCList[LineNo].Y1);
				((ListControl)cmbM1).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].M1);
				((ListControl)cmbD1).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].D1);
			}
			else
			{
				updnY1.set_Value(0m);
				ComboBox obj2 = cmbM1;
				int selectedIndex;
				((ListControl)cmbD1).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj2).set_SelectedIndex(selectedIndex);
			}
			if (EditXZ_Forms.OCCList[LineNo].Y2 > 0)
			{
				updnY2.set_Value((decimal)EditXZ_Forms.OCCList[LineNo].Y2);
				((ListControl)cmbM2).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].M2);
				((ListControl)cmbD2).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].D2);
			}
			else
			{
				updnY2.set_Value(0m);
				ComboBox obj3 = cmbM2;
				int selectedIndex;
				((ListControl)cmbD2).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj3).set_SelectedIndex(selectedIndex);
			}
			if (EditXZ_Forms.OCCList[LineNo].Y3 > 0)
			{
				updnY3.set_Value((decimal)EditXZ_Forms.OCCList[LineNo].Y3);
				((ListControl)cmbM3).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].M3);
				((ListControl)cmbD3).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].D3);
			}
			else
			{
				updnY3.set_Value(0m);
				ComboBox obj4 = cmbM3;
				int selectedIndex;
				((ListControl)cmbD3).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj4).set_SelectedIndex(selectedIndex);
			}
			if (EditXZ_Forms.OCCList[LineNo].Y4 > 0)
			{
				updnY4.set_Value((decimal)EditXZ_Forms.OCCList[LineNo].Y4);
				((ListControl)cmbM4).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].M4);
				((ListControl)cmbD4).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].D4);
			}
			else
			{
				updnY4.set_Value(0m);
				ComboBox obj5 = cmbM4;
				int selectedIndex;
				((ListControl)cmbD4).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj5).set_SelectedIndex(selectedIndex);
			}
			if (EditXZ_Forms.OCCList[LineNo].Y5 > 0)
			{
				updnY5.set_Value((decimal)EditXZ_Forms.OCCList[LineNo].Y5);
				((ListControl)cmbM5).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].M5);
				((ListControl)cmbD5).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].D5);
			}
			else
			{
				updnY5.set_Value(0m);
				ComboBox obj6 = cmbM5;
				int selectedIndex;
				((ListControl)cmbD5).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj6).set_SelectedIndex(selectedIndex);
			}
			((ListControl)cmbDoubleStatus).set_SelectedIndex(EditXZ_Forms.OCCList[LineNo].Status);
			((ListControl)cmbLightCurve).set_SelectedIndex(" ?*".IndexOf(EditXZ_Forms.OCCList[LineNo].LightCurve));
		}

		private void lstOCC_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!Initialised)
			{
				return;
			}
			DecodeOCCLine(((ListControl)lstOCC).get_SelectedIndex());
			((Control)cmdAddOCC).set_Enabled(false);
			((Control)cmdReplaceOCC).set_Enabled(true);
			if (autosearchWDSInterferometricToolStripMenuItem.get_Checked())
			{
				if (((Control)panelDoubles).get_Visible())
				{
					cmdDoublesSearchInterferometer_Click(sender, e);
				}
				else
				{
					button1_Click(sender, e);
				}
			}
		}

		private void cmdNameFromList_Click(object sender, EventArgs e)
		{
			((Control)lstNames).set_Visible(true);
		}

		private void lstNames_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex > 0)
			{
				((Control)txtDiscoverer).set_Text(lstNames.get_Items().get_Item(selectedIndex).ToString());
			}
			((Control)lstNames).set_Visible(false);
		}

		private void txtZCinit_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtZCinit).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZCinit;
					TextBox obj2 = txtSAOinit;
					string text;
					((Control)txtXZinit).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else if ((result > 4000 && result < 4009) || (result > 4051 && result <= 4054) || (result > 4061 && result <= 4068))
				{
					TextBox obj3 = txtSAOinit;
					string text2;
					((Control)txtXZinit).set_Text(text2 = "");
					((Control)obj3).set_Text(text2);
				}
				else
				{
					XZ80Q.Get_ZC_Star(result);
					((Control)txtSAOinit).set_Text(XZ80Q.SAO.ToString());
					((Control)txtXZinit).set_Text(XZ80Q.XZ.ToString());
				}
				ChangingStar = false;
			}
		}

		private void txtSAOinit_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtSAOinit).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZCinit;
					TextBox obj2 = txtSAOinit;
					string text;
					((Control)txtXZinit).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZCinit).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZinit).set_Text(XZ80Q.XZ.ToString());
				}
				else
				{
					TextBox obj3 = txtZCinit;
					string text2;
					((Control)txtXZinit).set_Text(text2 = "0");
					((Control)obj3).set_Text(text2);
				}
				ChangingStar = false;
			}
		}

		private void txtXZinit_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtXZinit).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZCinit;
					TextBox obj2 = txtSAOinit;
					string text;
					((Control)txtXZinit).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else
				{
					XZ80Q.Get_XZ_Star(result);
					((Control)txtZCinit).set_Text(XZ80Q.ZC.ToString());
					((Control)txtSAOinit).set_Text(XZ80Q.SAO.ToString());
				}
				ChangingStar = false;
			}
		}

		private void cmdOCCinit_Click(object sender, EventArgs e)
		{
			if (XZ80Q.XZ >= 1)
			{
				((Control)cmdAddOCC).set_Enabled(true);
				((Control)cmdReplaceOCC).set_Enabled(false);
				string text = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true);
				((Control)txtOCCRAH).set_Text(text.Substring(0, 2).Trim());
				((Control)txtOCCRAM).set_Text(text.Substring(3).Trim());
				string text2 = Utilities.DEGtoDMS(XZ80Q.Dec_rad * (180.0 / Math.PI), 3, 0, MinutesOnly: true);
				((Control)txtOCCDecD).set_Text(text2.Substring(0, 3).Trim());
				((Control)txtOCCDecM).set_Text(text2.Substring(4).Trim());
				((Control)txtOCCXZ).set_Text(XZ80Q.XZ.ToString());
				((Control)txtOCC).set_Text((lstOCC.get_Items().get_Count() + 1).ToString());
				TextBox obj = txtOCCBDNumber;
				TextBox obj2 = txtOCCBDZone;
				TextBox obj3 = txtOCCCode;
				TextBox obj4 = txtOCCNumber;
				TextBox obj5 = txtOCCPair;
				TextBox obj6 = txtOCCRef;
				TextBox obj7 = txtDiscoverer;
				string text3;
				((Control)txtReference).set_Text(text3 = "");
				string text4;
				((Control)obj7).set_Text(text4 = text3);
				string text5;
				((Control)obj6).set_Text(text5 = text4);
				string text6;
				((Control)obj5).set_Text(text6 = text5);
				string text7;
				((Control)obj4).set_Text(text7 = text6);
				string text8;
				((Control)obj3).set_Text(text8 = text7);
				string text9;
				((Control)obj2).set_Text(text9 = text8);
				((Control)obj).set_Text(text9);
				((ListControl)cmbDoubleStatus).set_SelectedIndex(1);
				updnY1.set_Value((decimal)DateTime.Now.Year);
				((ListControl)cmbM1).set_SelectedIndex(1);
				((ListControl)cmbD1).set_SelectedIndex(1);
				NumericUpDown obj8 = updnY2;
				NumericUpDown obj9 = updnY3;
				NumericUpDown obj10 = updnY4;
				NumericUpDown obj11 = updnY5;
				decimal num = default(decimal);
				obj11.set_Value(num);
				decimal num2;
				obj10.set_Value(num2 = num);
				decimal value;
				obj9.set_Value(value = num2);
				obj8.set_Value(value);
				ComboBox obj12 = cmbM2;
				ComboBox obj13 = cmbM3;
				ComboBox obj14 = cmbM4;
				int num3;
				((ListControl)cmbM5).set_SelectedIndex(num3 = 0);
				int num4;
				((ListControl)obj14).set_SelectedIndex(num4 = num3);
				int selectedIndex;
				((ListControl)obj13).set_SelectedIndex(selectedIndex = num4);
				((ListControl)obj12).set_SelectedIndex(selectedIndex);
				ComboBox obj15 = cmbD2;
				ComboBox obj16 = cmbD3;
				ComboBox obj17 = cmbD4;
				((ListControl)cmbD5).set_SelectedIndex(num3 = 0);
				((ListControl)obj17).set_SelectedIndex(num4 = num3);
				((ListControl)obj16).set_SelectedIndex(selectedIndex = num4);
				((ListControl)obj15).set_SelectedIndex(selectedIndex);
			}
		}

		internal void EncodeOCCLine(int Line)
		{
			int result = 0;
			int result2 = 0;
			if (!int.TryParse(((Control)txtOCC).get_Text(), out result))
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].OCC = result;
			if (!int.TryParse(((Control)txtOCCXZ).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 244437 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].XZ = result;
			if (!int.TryParse(((Control)txtOCCRAH).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 23 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].RAH = result;
			if (!int.TryParse(((Control)txtOCCRAM).get_Text().Replace(".", ""), out result))
			{
				result = 0;
			}
			if (result > 599 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].RAM = result;
			EditXZ_Forms.OCCList[Line].DecSign = !((Control)txtOCCDecD).get_Text().Contains("-");
			if (!int.TryParse(((Control)txtOCCDecD).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			if (result > 89)
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].DecD = result;
			if (!int.TryParse(((Control)txtOCCDecM).get_Text(), out result))
			{
				result = 0;
			}
			if (result > 59 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.OCCList[Line].DecM = result;
			EditXZ_Forms.OCCList[Line].IDCode = ((Control)txtOCCCode).get_Text().Trim().PadRight(3)
				.Substring(0, 3);
			if (int.TryParse(((Control)txtOCCNumber).get_Text(), out result))
			{
				EditXZ_Forms.OCCList[Line].IDNumber = ((Control)txtOCCNumber).get_Text().Trim().PadLeft(4)
					.Substring(0, 4);
			}
			else
			{
				EditXZ_Forms.OCCList[Line].IDNumber = ((Control)txtOCCNumber).get_Text().Trim().PadRight(4)
					.Substring(0, 4);
			}
			EditXZ_Forms.OCCList[Line].IDPair = ((Control)txtOCCPair).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			if (!int.TryParse(((Control)txtOCCBDZone).get_Text().Replace("-", ""), out result))
			{
				result = -99;
			}
			if (!int.TryParse(((Control)txtOCCBDNumber).get_Text(), out result2))
			{
				result2 = -1;
			}
			if (result > -90 && result2 > 0 && result2 < 99999)
			{
				EditXZ_Forms.OCCList[Line].BDZone = result;
				EditXZ_Forms.OCCList[Line].BDNumber = result2;
				EditXZ_Forms.OCCList[Line].BDSign = !((Control)txtOCCBDZone).get_Text().Contains("-");
			}
			else
			{
				EditXZ_Forms.OCCList[Line].BDZone = -99;
				EditXZ_Forms.OCCList[Line].BDNumber = -1;
				EditXZ_Forms.OCCList[Line].BDSign = true;
			}
			EditXZ_Forms.OCCList[Line].dsFile = ((Control)txtOCCRef).get_Text().Trim().PadRight(7)
				.Substring(0, 7);
			EditXZ_Forms.OCCList[Line].Discoverer = ((Control)txtDiscoverer).get_Text().Trim().PadRight(24)
				.Substring(0, 24);
			EditXZ_Forms.OCCList[Line].Reference = ((Control)txtReference).get_Text().Trim().PadRight(24)
				.Substring(0, 24);
			EditXZ_Forms.OCCList[Line].Y1 = (int)updnY1.get_Value();
			EditXZ_Forms.OCCList[Line].M1 = ((ListControl)cmbM1).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].D1 = ((ListControl)cmbD1).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].Y2 = (int)updnY2.get_Value();
			EditXZ_Forms.OCCList[Line].M2 = ((ListControl)cmbM2).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].D2 = ((ListControl)cmbD2).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].Y3 = (int)updnY3.get_Value();
			EditXZ_Forms.OCCList[Line].M3 = ((ListControl)cmbM3).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].D3 = ((ListControl)cmbD3).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].Y4 = (int)updnY4.get_Value();
			EditXZ_Forms.OCCList[Line].M4 = ((ListControl)cmbM4).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].D4 = ((ListControl)cmbD4).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].Y5 = (int)updnY5.get_Value();
			EditXZ_Forms.OCCList[Line].M5 = ((ListControl)cmbM5).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].D5 = ((ListControl)cmbD5).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].Status = ((ListControl)cmbDoubleStatus).get_SelectedIndex();
			EditXZ_Forms.OCCList[Line].LightCurve = " ?*".Substring(((ListControl)cmbLightCurve).get_SelectedIndex());
		}

		private void cmdAddOCC_Click(object sender, EventArgs e)
		{
			OCCstars item = new OCCstars();
			EditXZ_Forms.OCCList.Add(item);
			int num = EditXZ_Forms.OCCList.Count - 1;
			EncodeOCCLine(num);
			PopulateOCCList(num);
			OCCChanged = true;
			TextBox obj = txtXZprimary;
			string text;
			((Control)txtXZprinciple).set_Text(text = ((Control)txtOCCXZ).get_Text());
			((Control)obj).set_Text(text);
			((Control)txtXZsecondary).set_Text("");
			((Control)txtRAH).set_Text(((Control)txtOCCRAH).get_Text());
			((Control)txtRAM).set_Text(((Control)txtOCCRAM).get_Text());
			((Control)txtDecD).set_Text(((Control)txtOCCDecD).get_Text());
			((Control)txtDecM).set_Text(((Control)txtOCCDecM).get_Text());
			if (((ListControl)cmbDoubleStatus).get_SelectedIndex() == 2)
			{
				((Control)txtCode).set_Text("OC*");
			}
			else if (((ListControl)cmbDoubleStatus).get_SelectedIndex() == 1)
			{
				((Control)txtCode).set_Text("OCc");
			}
			else
			{
				((Control)txtCode).set_Text("OCC");
			}
			((Control)txtNumber).set_Text(((Control)txtOCC).get_Text());
			((Control)txtPair).set_Text("");
			((Control)txtBDZone).set_Text(((Control)txtOCCBDZone).get_Text());
			((Control)txtBDNumber).set_Text(((Control)txtOCCBDNumber).get_Text());
			((Control)txtY1).set_Text(updnY1.get_Value().ToString());
			((Control)txtY2).set_Text("");
			TextBox obj2 = txtPA1;
			TextBox obj3 = txtPA2;
			TextBox obj4 = txtSep1;
			string text2;
			((Control)txtSep2).set_Text(text2 = "");
			string text3;
			((Control)obj4).set_Text(text3 = text2);
			((Control)obj3).set_Text(text = text3);
			((Control)obj2).set_Text(text);
			ComboBox obj5 = cmbSep1_LessThan;
			int selectedIndex;
			((ListControl)cmbSep2_LessThan).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj5).set_SelectedIndex(selectedIndex);
			((Control)txtNumObs).set_Text("1");
			((Control)txtMag1).set_Text(XZ80Q.Mv.ToString());
			((Control)txtMag2).set_Text("");
			((Control)txtSpectrum).set_Text(XZ80Q.Spectrum);
			ComboBox obj6 = cmbWDS1;
			ComboBox obj7 = cmbWDS2;
			int num2;
			((ListControl)cmbWDS3).set_SelectedIndex(num2 = 0);
			((ListControl)obj7).set_SelectedIndex(selectedIndex = num2);
			((ListControl)obj6).set_SelectedIndex(selectedIndex);
			TextBox obj8 = txtP;
			TextBox obj9 = txtA;
			TextBox obj10 = txtI;
			TextBox obj11 = txtNode;
			TextBox obj12 = txtT;
			TextBox obj13 = txtE;
			string text4;
			((Control)txtPeri).set_Text(text4 = "");
			string text5;
			((Control)obj13).set_Text(text5 = text4);
			string text6;
			((Control)obj12).set_Text(text6 = text5);
			((Control)obj11).set_Text(text2 = text6);
			((Control)obj10).set_Text(text3 = text2);
			((Control)obj9).set_Text(text = text3);
			((Control)obj8).set_Text(text);
			((Control)panelDoubles).set_Visible(true);
			((Control)panelOCC).set_Visible(false);
		}

		private void cmdReplaceOCC_Click(object sender, EventArgs e)
		{
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstOCC).get_SelectedIndex();
			if ((int)MessageBox.Show("Do you want to replace\r\n\r\n" + lstOCC.get_Items().get_Item(selectedIndex).ToString(), "Confirm Replace", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				EncodeOCCLine(selectedIndex);
				PopulateOCCList(selectedIndex);
				OCCChanged = true;
			}
		}

		private void txtCode_TextChanged(object sender, EventArgs e)
		{
			((Control)lblCode).set_Text(DoubleStars.WDSName(((Control)txtCode).get_Text()).PadRight(5).Substring(5));
		}

		private void lstDoubles_DoubleClick(object sender, EventArgs e)
		{
			GetWDSandIFFromXZEditor();
		}

		private void cmdDoublesSearchInterferometer_Click(object sender, EventArgs e)
		{
			GetWDSandIFFromXZEditor();
		}

		private void GetWDSandIFFromXZEditor()
		{
			if (!double.TryParse(((Control)txtRAH).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtRAM).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			double rA_hrs = result + result2 / 60.0;
			if (!double.TryParse(((Control)txtDecD).get_Text().Replace("-", ""), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(((Control)txtDecM).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			double num = result3 + result4 / 60.0;
			if (((Control)txtDecD).get_Text().Contains("-"))
			{
				num = 0.0 - num;
			}
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(rA_hrs, num, HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void button1_Click(object sender, EventArgs e)
		{
			GetWDSandIFFromOCCEditor();
		}

		private void lstOCC_DoubleClick(object sender, EventArgs e)
		{
			GetWDSandIFFromOCCEditor();
		}

		private void GetWDSandIFFromOCCEditor()
		{
			if (!double.TryParse(((Control)txtOCCRAH).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtOCCRAM).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			double rA_hrs = result + result2 / 60.0;
			if (!double.TryParse(((Control)txtOCCDecD).get_Text().Replace("-", ""), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(((Control)txtOCCDecM).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			double num = result3 + result4 / 60.0;
			if (((Control)txtOCCDecD).get_Text().Contains("-"))
			{
				num = 0.0 - num;
			}
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(rA_hrs, num, HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void cmdCompareAllOCCwithIF_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.ShowIFMismatches();
			Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Clear();
			for (int i = 0; i < 3; i++)
			{
				switch (i)
				{
				case 0:
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"The Interferometric catalogue indicates following ");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"OCC stars may be definitely double");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					break;
				case 1:
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"The following OCC stars only have an occultation entry");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"(or no entry) in the Interferometric catalogue, but a");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"light curve shows the star to be definitely double");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					break;
				default:
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"The Interferometric catalogue does not provide any");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"independent determination that the following ");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"OCC stars are definitely double");
					Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)"");
					break;
				}
				for (int j = 0; j < EditXZ_Forms.OCCList.Count; j++)
				{
					double rA_hrs = (double)EditXZ_Forms.OCCList[j].RAH + (double)EditXZ_Forms.OCCList[j].RAM / 600.0;
					double num = (double)EditXZ_Forms.OCCList[j].DecD + (double)EditXZ_Forms.OCCList[j].DecM / 60.0;
					if (!EditXZ_Forms.OCCList[j].DecSign)
					{
						num = 0.0 - num;
					}
					bool flag = Interferometric_Plus_WDS.IsPositivelyDoubleInInterferometric(rA_hrs, num, HighPrecision: false);
					if (((i == 0) & (EditXZ_Forms.OCCList[j].Status == 1)) && flag)
					{
						Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)("OCC" + EditXZ_Forms.OCCList[j].OCC));
					}
					else if ((((i == 1) & (EditXZ_Forms.OCCList[j].Status == 0)) && !flag) & (EditXZ_Forms.OCCList[j].LightCurve == "*"))
					{
						Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)("OCC" + EditXZ_Forms.OCCList[j].OCC));
					}
					else if (((i == 2) & (EditXZ_Forms.OCCList[j].Status == 0)) && !flag)
					{
						Interferometric_Plus_WDS.Mismatch.lstMisMatches.get_Items().Add((object)("OCC" + EditXZ_Forms.OCCList[j].OCC + "   " + EditXZ_Forms.OCCList[j].Reference.Trim()));
					}
				}
			}
			((Control)Interferometric_Plus_WDS.Mismatch).Focus();
		}

		private void cmdFindWDSMissingFromXZDoubles_Click(object sender, EventArgs e)
		{
			WDS_Entries_Added_To_XZDoubles = Access_WDS_by_Name.AddNewWDS_EntriesToXZDoubles();
			if (WDS_Entries_Added_To_XZDoubles)
			{
				DoublesChanged = true;
			}
		}

		private void cmdGoToNextAdded_Click(object sender, EventArgs e)
		{
			//IL_0088: Unknown result type (might be due to invalid IL or missing references)
			//IL_008e: Invalid comparison between Unknown and I4
			int num = ((ListControl)lstDoubles).get_SelectedIndex();
			do
			{
				num++;
				if (num >= lstDoubles.get_Items().get_Count())
				{
					break;
				}
				if (EditXZ_Forms.DoubleList[num].LineAdded)
				{
					((ListControl)lstDoubles).set_SelectedIndex(num);
					break;
				}
			}
			while (num < lstDoubles.get_Items().get_Count());
			if ((WDS_Entries_Added_To_XZDoubles & (num >= lstDoubles.get_Items().get_Count())) && (int)MessageBox.Show("You have added/amended entries in XZDoubles.\r\nTo continue, you need to Save, and re-read XZDoubles.\r\n\r\n. Do you want to Save, then re-Read, XZDoubles now?", "Save and re-Read", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				Save_and_ReRead();
			}
			((Control)EditXZ_Forms.DoubleEditor.lstDoubles).Focus();
		}

		private void cmdAddToOCC_Click(object sender, EventArgs e)
		{
			string iDCode = "OCc";
			for (int i = 0; i < EditXZ_Forms.DoubleList.Count; i++)
			{
				if (EditXZ_Forms.DoubleList[i].IDCode == "S  " || EditXZ_Forms.DoubleList[i].IDCode == "GCB" || !((EditXZ_Forms.DoubleList[i].IDCode == "---") | (EditXZ_Forms.DoubleList[i].IDCode == "   ") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "S ") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "S1") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "GC") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "BD") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 1) == "+")))
				{
					continue;
				}
				OCCstars oCCstars = new OCCstars();
				oCCstars.OCC = EditXZ_Forms.OCCList.Count + 1;
				oCCstars.XZ = EditXZ_Forms.DoubleList[i].XZPrimary;
				oCCstars.RAH = EditXZ_Forms.DoubleList[i].RAH;
				oCCstars.RAM = EditXZ_Forms.DoubleList[i].RAM;
				oCCstars.DecSign = EditXZ_Forms.DoubleList[i].DecSign;
				oCCstars.DecD = EditXZ_Forms.DoubleList[i].DecD;
				oCCstars.DecM = EditXZ_Forms.DoubleList[i].DecM;
				oCCstars.BDSign = EditXZ_Forms.DoubleList[i].BDSign;
				oCCstars.BDZone = EditXZ_Forms.DoubleList[i].BDZone;
				oCCstars.BDNumber = EditXZ_Forms.DoubleList[i].BDNumber;
				oCCstars.Y1 = EditXZ_Forms.DoubleList[i].Y1;
				oCCstars.M1 = 1;
				oCCstars.D1 = 1;
				if (EditXZ_Forms.DoubleList[i].IDCode == "---")
				{
					oCCstars.Status = 1;
					oCCstars.Discoverer = "Unknown";
					iDCode = "OCc";
					if (oCCstars.Y1 < 100)
					{
						oCCstars.Y1 = 1980;
					}
				}
				else if (EditXZ_Forms.DoubleList[i].IDCode == "   ")
				{
					if ((EditXZ_Forms.DoubleList[i].WDSNote2 == "X") & (EditXZ_Forms.DoubleList[i].WDSNote3 == "Z"))
					{
						oCCstars.Status = 0;
						oCCstars.Discoverer = "Unknown";
						oCCstars.Reference = "close in XZ catalogue";
						iDCode = "OCC";
						if (oCCstars.Y1 < 100)
						{
							oCCstars.Y1 = 2000;
						}
					}
					else
					{
						oCCstars.Status = 1;
						oCCstars.Discoverer = "Unknown";
						iDCode = "OCc";
						if (oCCstars.Y1 < 100)
						{
							oCCstars.Y1 = 1980;
						}
					}
				}
				else if ((EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "S ") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "S1") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "GC") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 2) == "BD") | (EditXZ_Forms.DoubleList[i].IDCode.Substring(0, 1) == "+"))
				{
					oCCstars.Status = 1;
					oCCstars.Discoverer = "Unknown";
					oCCstars.Reference = EditXZ_Forms.DoubleList[i].IDCode + EditXZ_Forms.DoubleList[i].IDNumber;
					iDCode = "OCc";
					if (oCCstars.Y1 < 100)
					{
						oCCstars.Y1 = 1970;
					}
				}
				EditXZ_Forms.DoubleList[i].IDCode = iDCode;
				EditXZ_Forms.DoubleList[i].IDNumber = oCCstars.OCC.ToString();
				EditXZ_Forms.OCCList.Add(oCCstars);
			}
			PopulateOCCList(EditXZ_Forms.OCCList.Count - 2);
			PopulateDoublesList(0);
		}

		private void addEditOCCStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			EditOCCstars();
		}

		private void updateOCCStarEntriesInXZDoublesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SyncXZDoublesWithOCCstars();
		}

		private void createFileOfOCCStarsInXZDoublesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_003b: Invalid comparison between Unknown and I4
			//IL_005d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Invalid comparison between Unknown and I4
			List<OccStarEntries> list = new List<OccStarEntries>();
			string path = Utilities.AppPath + "\\Resource Files\\XZDoubles_OCC_additions.dat";
			if ((File.Exists(path) && (int)MessageBox.Show("A file of OCC star additions already exists. If you continue,\r\nthat file will be overwritten - and all entries that are \r\nnot in your current XZDoubles.dat file will be lost.\r\n\r\nDo you want to continue?", "File exists", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7) || (DoublesChanged && (int)MessageBox.Show("Changes to XZDoubles have not been saved. As a\r\nresult, the file that is created might not be up-to-date,\r\n\r\nDo you want to continue with the closing the form?", "Check unsaved changes", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat"))
			{
				do
				{
					string text = streamReader.ReadLine();
					if ((text.Substring(32, 3).ToUpper() == "OCC") | (text.Substring(32, 3).ToUpper() == "OC*"))
					{
						OccStarEntries occStarEntries = new OccStarEntries();
						occStarEntries.XZ_OCCentry(text);
						if (occStarEntries.OCCNumber < 9000)
						{
							list.Add(occStarEntries);
						}
					}
				}
				while (!streamReader.EndOfStream);
			}
			list.Sort();
			using StreamWriter streamWriter = new StreamWriter(path, append: false);
			for (int i = 0; i < list.Count; i++)
			{
				streamWriter.WriteLine(list[i].OCCline.ToString());
			}
		}

		private void addNewOCCStarsToXZDoublesFromFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_013c: Unknown result type (might be due to invalid IL or missing references)
			int count = EditXZ_Forms.DoubleList.Count;
			int num = 0;
			int result = 0;
			for (int i = 0; i < count; i++)
			{
				if (EditXZ_Forms.DoubleList[i].IDCode.ToUpper() == "OCC")
				{
					if (!int.TryParse(EditXZ_Forms.DoubleList[i].IDNumber, out result))
					{
						result = -1;
					}
					if (result > num && result < 9000)
					{
						num = result;
					}
				}
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles_OCC_additions.dat"))
			{
				do
				{
					string text = streamReader.ReadLine();
					if ((text.Substring(32, 3).ToUpper() == "OCC") | (text.Substring(32, 3).ToUpper() == "OC*"))
					{
						if (!int.TryParse(text.Substring(35, 4), out var result2))
						{
							result2 = -1;
						}
						if (result2 > num && result2 < 9000)
						{
							XZDoubles xZDoubles = new XZDoubles();
							xZDoubles.DecodeLine(text);
							EditXZ_Forms.DoubleList.Add(xZDoubles);
						}
					}
				}
				while (!streamReader.EndOfStream);
			}
			PopulateDoublesList(EditXZ_Forms.DoubleList.Count - 1);
			DoublesChanged = true;
			MessageBox.Show("New OCC stars have been added to the end of the list.\r\nAfter reviewing the additions, Sort then Save the list", "OCC stars added");
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
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_0555: Unknown result type (might be due to invalid IL or missing references)
			//IL_055f: Expected O, but got Unknown
			//IL_0560: Unknown result type (might be due to invalid IL or missing references)
			//IL_056a: Expected O, but got Unknown
			//IL_056b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0575: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_05d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e3: Expected O, but got Unknown
			//IL_05e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ee: Expected O, but got Unknown
			//IL_05ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f9: Expected O, but got Unknown
			//IL_05fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0604: Expected O, but got Unknown
			//IL_0605: Unknown result type (might be due to invalid IL or missing references)
			//IL_060f: Expected O, but got Unknown
			//IL_0610: Unknown result type (might be due to invalid IL or missing references)
			//IL_061a: Expected O, but got Unknown
			//IL_061b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0625: Expected O, but got Unknown
			//IL_0626: Unknown result type (might be due to invalid IL or missing references)
			//IL_0630: Expected O, but got Unknown
			//IL_0631: Unknown result type (might be due to invalid IL or missing references)
			//IL_063b: Expected O, but got Unknown
			//IL_063c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0646: Expected O, but got Unknown
			//IL_0647: Unknown result type (might be due to invalid IL or missing references)
			//IL_0651: Expected O, but got Unknown
			//IL_0652: Unknown result type (might be due to invalid IL or missing references)
			//IL_065c: Expected O, but got Unknown
			//IL_065d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0667: Expected O, but got Unknown
			//IL_0668: Unknown result type (might be due to invalid IL or missing references)
			//IL_0672: Expected O, but got Unknown
			//IL_0673: Unknown result type (might be due to invalid IL or missing references)
			//IL_067d: Expected O, but got Unknown
			//IL_067e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0688: Expected O, but got Unknown
			//IL_0689: Unknown result type (might be due to invalid IL or missing references)
			//IL_0693: Expected O, but got Unknown
			//IL_0694: Unknown result type (might be due to invalid IL or missing references)
			//IL_069e: Expected O, but got Unknown
			//IL_069f: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a9: Expected O, but got Unknown
			//IL_06aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b4: Expected O, but got Unknown
			//IL_06b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06bf: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_06cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d5: Expected O, but got Unknown
			//IL_06d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e0: Expected O, but got Unknown
			//IL_06e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06eb: Expected O, but got Unknown
			//IL_06ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f6: Expected O, but got Unknown
			//IL_06f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0701: Expected O, but got Unknown
			//IL_0702: Unknown result type (might be due to invalid IL or missing references)
			//IL_070c: Expected O, but got Unknown
			//IL_070d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0717: Expected O, but got Unknown
			//IL_0718: Unknown result type (might be due to invalid IL or missing references)
			//IL_0722: Expected O, but got Unknown
			//IL_0723: Unknown result type (might be due to invalid IL or missing references)
			//IL_072d: Expected O, but got Unknown
			//IL_072e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0738: Expected O, but got Unknown
			//IL_0739: Unknown result type (might be due to invalid IL or missing references)
			//IL_0743: Expected O, but got Unknown
			//IL_0744: Unknown result type (might be due to invalid IL or missing references)
			//IL_074e: Expected O, but got Unknown
			//IL_074f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0759: Expected O, but got Unknown
			//IL_075a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0764: Expected O, but got Unknown
			//IL_0765: Unknown result type (might be due to invalid IL or missing references)
			//IL_076f: Expected O, but got Unknown
			//IL_0770: Unknown result type (might be due to invalid IL or missing references)
			//IL_077a: Expected O, but got Unknown
			//IL_077b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0785: Expected O, but got Unknown
			//IL_0786: Unknown result type (might be due to invalid IL or missing references)
			//IL_0790: Expected O, but got Unknown
			//IL_0791: Unknown result type (might be due to invalid IL or missing references)
			//IL_079b: Expected O, but got Unknown
			//IL_079c: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a6: Expected O, but got Unknown
			//IL_07a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b1: Expected O, but got Unknown
			//IL_07b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07bc: Expected O, but got Unknown
			//IL_07bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c7: Expected O, but got Unknown
			//IL_07c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d2: Expected O, but got Unknown
			//IL_07d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07dd: Expected O, but got Unknown
			//IL_07de: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e8: Expected O, but got Unknown
			//IL_07e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f3: Expected O, but got Unknown
			//IL_07f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fe: Expected O, but got Unknown
			//IL_07ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0809: Expected O, but got Unknown
			//IL_080a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0814: Expected O, but got Unknown
			//IL_0815: Unknown result type (might be due to invalid IL or missing references)
			//IL_081f: Expected O, but got Unknown
			//IL_0820: Unknown result type (might be due to invalid IL or missing references)
			//IL_082a: Expected O, but got Unknown
			//IL_082b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0835: Expected O, but got Unknown
			//IL_0836: Unknown result type (might be due to invalid IL or missing references)
			//IL_0840: Expected O, but got Unknown
			//IL_0841: Unknown result type (might be due to invalid IL or missing references)
			//IL_084b: Expected O, but got Unknown
			//IL_084c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0856: Expected O, but got Unknown
			//IL_0857: Unknown result type (might be due to invalid IL or missing references)
			//IL_0861: Expected O, but got Unknown
			//IL_0862: Unknown result type (might be due to invalid IL or missing references)
			//IL_086c: Expected O, but got Unknown
			//IL_086d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0877: Expected O, but got Unknown
			//IL_0878: Unknown result type (might be due to invalid IL or missing references)
			//IL_0882: Expected O, but got Unknown
			//IL_0883: Unknown result type (might be due to invalid IL or missing references)
			//IL_088d: Expected O, but got Unknown
			//IL_088e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0898: Expected O, but got Unknown
			//IL_0899: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a3: Expected O, but got Unknown
			//IL_08a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ae: Expected O, but got Unknown
			//IL_08af: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b9: Expected O, but got Unknown
			//IL_08ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c4: Expected O, but got Unknown
			//IL_6604: Unknown result type (might be due to invalid IL or missing references)
			//IL_660e: Expected O, but got Unknown
			//IL_7404: Unknown result type (might be due to invalid IL or missing references)
			//IL_740e: Expected O, but got Unknown
			//IL_747c: Unknown result type (might be due to invalid IL or missing references)
			//IL_7486: Expected O, but got Unknown
			//IL_748e: Unknown result type (might be due to invalid IL or missing references)
			//IL_7498: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			autosearchWDSInterferometricToolStripMenuItem = new ToolStripMenuItem();
			oCCStarMaintenanceToolStripMenuItem = new ToolStripMenuItem();
			addEditOCCStarsToolStripMenuItem = new ToolStripMenuItem();
			updateOCCStarEntriesInXZDoublesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			createFileOfOCCStarsInXZDoublesToolStripMenuItem = new ToolStripMenuItem();
			addNewOCCStarsToXZDoublesFromFileToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstDoubles = new ListBox();
			groupBox1 = new GroupBox();
			chkMeanPos = new CheckBox();
			label37 = new Label();
			txtBDNumber = new TextBox();
			txtBDZone = new TextBox();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			label14 = new Label();
			label13 = new Label();
			label12 = new Label();
			label11 = new Label();
			label10 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			label6 = new Label();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			label1 = new Label();
			txtRAH = new TextBox();
			txtXZsecondary = new TextBox();
			txtXZprinciple = new TextBox();
			txtRAM = new TextBox();
			txtDecD = new TextBox();
			txtDecM = new TextBox();
			txtCode = new TextBox();
			txtPMDec = new TextBox();
			txtPMRA = new TextBox();
			txtPair = new TextBox();
			txtNumber = new TextBox();
			txtXZprimary = new TextBox();
			label2 = new Label();
			lblCode = new Label();
			groupBox2 = new GroupBox();
			cmbSep2_LessThan = new ComboBox();
			cmbSep1_LessThan = new ComboBox();
			label32 = new Label();
			label31 = new Label();
			label30 = new Label();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			label25 = new Label();
			label22 = new Label();
			label20 = new Label();
			label19 = new Label();
			cmbWDS3 = new ComboBox();
			cmbWDS2 = new ComboBox();
			cmbWDS1 = new ComboBox();
			txtY1 = new TextBox();
			txtY2 = new TextBox();
			txtPA1 = new TextBox();
			txtPA2 = new TextBox();
			txtSep1 = new TextBox();
			txtSep2 = new TextBox();
			txtNumObs = new TextBox();
			txtMag1 = new TextBox();
			txtMag2 = new TextBox();
			txtSpectrum = new TextBox();
			groupBox3 = new GroupBox();
			label36 = new Label();
			label35 = new Label();
			label34 = new Label();
			label33 = new Label();
			txtI = new TextBox();
			txtA = new TextBox();
			txtP = new TextBox();
			txtPeri = new TextBox();
			txtE = new TextBox();
			txtT = new TextBox();
			txtNode = new TextBox();
			label21 = new Label();
			label23 = new Label();
			label24 = new Label();
			cmdFind = new Button();
			cmdAdd = new Button();
			cmdReplace = new Button();
			cmdDelete = new Button();
			txtXZSearch = new TextBox();
			label39 = new Label();
			label38 = new Label();
			label40 = new Label();
			txtIDSearchCode = new TextBox();
			txtIDSearchNumber = new TextBox();
			cmdFindID = new Button();
			panelDoubles = new Panel();
			cmdRetrieveWDS = new Button();
			groupBox8 = new GroupBox();
			cmdAddToOCC = new Button();
			cmdGoToNextAdded = new Button();
			cmdEditOCC = new Button();
			cmdFindWDSMissingFromXZDoubles = new Button();
			cmdCancel = new Button();
			cmdWDSUpdateALL = new Button();
			cmdSortXZDoubles = new Button();
			cmdDoublesSearchInterferometer = new Button();
			panelOCC = new Panel();
			cmbLightCurve = new ComboBox();
			label81 = new Label();
			cmdCompareAllOCCwithIF = new Button();
			button1 = new Button();
			cmbDoubleStatus = new ComboBox();
			label80 = new Label();
			cmdReplaceOCC = new Button();
			cmdAddOCC = new Button();
			groupBox7 = new GroupBox();
			cmdOCCinit = new Button();
			label79 = new Label();
			label78 = new Label();
			label77 = new Label();
			txtXZinit = new TextBox();
			txtSAOinit = new TextBox();
			txtZCinit = new TextBox();
			groupBox6 = new GroupBox();
			label76 = new Label();
			label75 = new Label();
			label74 = new Label();
			label73 = new Label();
			label70 = new Label();
			label71 = new Label();
			label72 = new Label();
			cmbD4 = new ComboBox();
			cmbM4 = new ComboBox();
			updnY4 = new NumericUpDown();
			label67 = new Label();
			label68 = new Label();
			label69 = new Label();
			cmbD3 = new ComboBox();
			cmbM3 = new ComboBox();
			updnY3 = new NumericUpDown();
			label64 = new Label();
			label65 = new Label();
			label66 = new Label();
			cmbD5 = new ComboBox();
			cmbM5 = new ComboBox();
			updnY5 = new NumericUpDown();
			label61 = new Label();
			label62 = new Label();
			label63 = new Label();
			cmbD2 = new ComboBox();
			cmbM2 = new ComboBox();
			updnY2 = new NumericUpDown();
			groupBox5 = new GroupBox();
			label51 = new Label();
			label58 = new Label();
			label60 = new Label();
			cmbD1 = new ComboBox();
			cmbM1 = new ComboBox();
			updnY1 = new NumericUpDown();
			label45 = new Label();
			txtReference = new TextBox();
			label43 = new Label();
			cmdNameFromList = new Button();
			txtDiscoverer = new TextBox();
			lstNames = new ListBox();
			groupBox4 = new GroupBox();
			label41 = new Label();
			txtOCCBDNumber = new TextBox();
			txtOCCBDZone = new TextBox();
			label42 = new Label();
			label44 = new Label();
			label46 = new Label();
			label47 = new Label();
			label48 = new Label();
			label49 = new Label();
			label50 = new Label();
			label52 = new Label();
			label53 = new Label();
			label54 = new Label();
			label55 = new Label();
			label56 = new Label();
			label57 = new Label();
			txtOCCRAH = new TextBox();
			txtOCCXZ = new TextBox();
			txtOCCRAM = new TextBox();
			txtOCCDecD = new TextBox();
			txtOCCDecM = new TextBox();
			txtOCCCode = new TextBox();
			txtOCCRef = new TextBox();
			txtOCCPair = new TextBox();
			txtOCCNumber = new TextBox();
			txtOCC = new TextBox();
			label59 = new Label();
			lstOCC = new ListBox();
			cmdToDoubles = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)panelDoubles).SuspendLayout();
			((Control)groupBox8).SuspendLayout();
			((Control)panelOCC).SuspendLayout();
			((Control)groupBox7).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((ISupportInitialize)updnY4).BeginInit();
			((ISupportInitialize)updnY3).BeginInit();
			((ISupportInitialize)updnY5).BeginInit();
			((ISupportInitialize)updnY2).BeginInit();
			((Control)groupBox5).SuspendLayout();
			((ISupportInitialize)updnY1).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)oCCStarMaintenanceToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1017, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)autosearchWDSInterferometricToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(82, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...            ");
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(259, 6));
			autosearchWDSInterferometricToolStripMenuItem.set_Checked(Settings.Default.AutoAsteroidDoubleInfo);
			autosearchWDSInterferometricToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)autosearchWDSInterferometricToolStripMenuItem).set_Name("autosearchWDSInterferometricToolStripMenuItem");
			((ToolStripItem)autosearchWDSInterferometricToolStripMenuItem).set_Size(new Size(262, 22));
			((ToolStripItem)autosearchWDSInterferometricToolStripMenuItem).set_Text("Auto-search WDS && Interferometric");
			((ToolStripDropDownItem)oCCStarMaintenanceToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)addEditOCCStarsToolStripMenuItem,
				(ToolStripItem)updateOCCStarEntriesInXZDoublesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)createFileOfOCCStarsInXZDoublesToolStripMenuItem,
				(ToolStripItem)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem
			});
			((ToolStripItem)oCCStarMaintenanceToolStripMenuItem).set_Name("oCCStarMaintenanceToolStripMenuItem");
			((ToolStripItem)oCCStarMaintenanceToolStripMenuItem).set_Size(new Size(165, 20));
			((ToolStripItem)oCCStarMaintenanceToolStripMenuItem).set_Text("OCC star maintenance...      ");
			((ToolStripItem)addEditOCCStarsToolStripMenuItem).set_Name("addEditOCCStarsToolStripMenuItem");
			((ToolStripItem)addEditOCCStarsToolStripMenuItem).set_Size(new Size(375, 22));
			((ToolStripItem)addEditOCCStarsToolStripMenuItem).set_Text("OCC star editor");
			((ToolStripItem)addEditOCCStarsToolStripMenuItem).add_Click((EventHandler)addEditOCCStarsToolStripMenuItem_Click);
			((ToolStripItem)updateOCCStarEntriesInXZDoublesToolStripMenuItem).set_Name("updateOCCStarEntriesInXZDoublesToolStripMenuItem");
			((ToolStripItem)updateOCCStarEntriesInXZDoublesToolStripMenuItem).set_Size(new Size(375, 22));
			((ToolStripItem)updateOCCStarEntriesInXZDoublesToolStripMenuItem).set_Text("Update XZDoubles using latest OCC data");
			((ToolStripItem)updateOCCStarEntriesInXZDoublesToolStripMenuItem).add_Click((EventHandler)updateOCCStarEntriesInXZDoublesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(372, 6));
			((ToolStripItem)createFileOfOCCStarsInXZDoublesToolStripMenuItem).set_Name("createFileOfOCCStarsInXZDoublesToolStripMenuItem");
			((ToolStripItem)createFileOfOCCStarsInXZDoublesToolStripMenuItem).set_Size(new Size(375, 22));
			((ToolStripItem)createFileOfOCCStarsInXZDoublesToolStripMenuItem).set_Text("Create 'Additions' file of XZDoubles data for all OCC stars");
			((ToolStripItem)createFileOfOCCStarsInXZDoublesToolStripMenuItem).add_Click((EventHandler)createFileOfOCCStarsInXZDoublesToolStripMenuItem_Click);
			((ToolStripItem)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem).set_Name("addNewOCCStarsToXZDoublesFromFileToolStripMenuItem");
			((ToolStripItem)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem).set_Size(new Size(375, 22));
			((ToolStripItem)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem).set_Text("Add OCC stars to XZDoubles, using 'Additions' file");
			((ToolStripItem)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem).add_Click((EventHandler)addNewOCCStarsToXZDoublesFromFileToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(90, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help          ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstDoubles).set_Font(new Font("Courier New", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDoubles).set_FormattingEnabled(true);
			lstDoubles.set_HorizontalScrollbar(true);
			lstDoubles.set_ItemHeight(12);
			((Control)lstDoubles).set_Location(new Point(2, 4));
			((Control)lstDoubles).set_Name("lstDoubles");
			((Control)lstDoubles).set_Size(new Size(991, 292));
			((Control)lstDoubles).set_TabIndex(0);
			lstDoubles.add_SelectedIndexChanged((EventHandler)lstDoubles_SelectedIndexChanged);
			((Control)lstDoubles).add_DoubleClick((EventHandler)lstDoubles_DoubleClick);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkMeanPos);
			((Control)groupBox1).get_Controls().Add((Control)(object)label37);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtBDNumber);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtBDZone);
			((Control)groupBox1).get_Controls().Add((Control)(object)label18);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)label16);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)label13);
			((Control)groupBox1).get_Controls().Add((Control)(object)label12);
			((Control)groupBox1).get_Controls().Add((Control)(object)label11);
			((Control)groupBox1).get_Controls().Add((Control)(object)label10);
			((Control)groupBox1).get_Controls().Add((Control)(object)label9);
			((Control)groupBox1).get_Controls().Add((Control)(object)label8);
			((Control)groupBox1).get_Controls().Add((Control)(object)label7);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtRAH);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtXZsecondary);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtXZprinciple);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtRAM);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtDecD);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtDecM);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtCode);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtPMDec);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtPMRA);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtPair);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtNumber);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtXZprimary);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblCode);
			((Control)groupBox1).set_Location(new Point(2, 297));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(968, 75));
			((Control)groupBox1).set_TabIndex(1);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Star identification details");
			((Control)chkMeanPos).set_AutoSize(true);
			chkMeanPos.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkMeanPos).set_Location(new Point(462, 9));
			((Control)chkMeanPos).set_Name("chkMeanPos");
			((Control)chkMeanPos).set_Size(new Size(38, 44));
			((Control)chkMeanPos).set_TabIndex(34);
			((Control)chkMeanPos).set_Text("Mean\r\nPosn");
			((ButtonBase)chkMeanPos).set_UseVisualStyleBackColor(true);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Location(new Point(867, 23));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(81, 13));
			((Control)label37).set_TabIndex(32);
			((Control)label37).set_Text("BD / CD / CPD");
			((Control)txtBDNumber).set_Location(new Point(897, 39));
			((Control)txtBDNumber).set_Name("txtBDNumber");
			((Control)txtBDNumber).set_Size(new Size(55, 20));
			((Control)txtBDNumber).set_TabIndex(31);
			((Control)txtBDZone).set_Location(new Point(862, 39));
			((Control)txtBDZone).set_Name("txtBDZone");
			((Control)txtBDZone).set_Size(new Size(28, 20));
			((Control)txtBDZone).set_TabIndex(30);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(314, 9));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(109, 13));
			((Control)label18).set_TabIndex(7);
			((Control)label18).set_Text("Star position   [J2000]");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(783, 24));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(27, 13));
			((Control)label17).set_TabIndex(28);
			((Control)label17).set_Text("Dec");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(686, 35));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(43, 26));
			((Control)label16).set_TabIndex(25);
			((Control)label16).set_Text("Proper\r\nmotions");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(735, 24));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(22, 13));
			((Control)label15).set_TabIndex(26);
			((Control)label15).set_Text("RA");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(527, 9));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(103, 13));
			((Control)label14).set_TabIndex(18);
			((Control)label14).set_Text("Double star identifier");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(8, 36));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(47, 26));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("XZ\r\nnumbers");
			label13.set_TextAlign(ContentAlignment.TopRight);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(278, 42));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(22, 13));
			((Control)label12).set_TabIndex(8);
			((Control)label12).set_Text("RA");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(372, 42));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(27, 13));
			((Control)label11).set_TabIndex(13);
			((Control)label11).set_Text("Dec");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(518, 24));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(32, 13));
			((Control)label10).set_TabIndex(19);
			((Control)label10).set_Text("Code");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(187, 11));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(41, 26));
			((Control)label9).set_TabIndex(5);
			((Control)label9).set_Text("System\r\nPrimary");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(123, 24));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(58, 13));
			((Control)label8).set_TabIndex(3);
			((Control)label8).set_Text("Secondary");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(556, 24));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(44, 13));
			((Control)label7).set_TabIndex(21);
			((Control)label7).set_Text("Number");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(606, 24));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(25, 13));
			((Control)label6).set_TabIndex(23);
			((Control)label6).set_Text("Pair");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(305, 24));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(9);
			((Control)label5).set_Text("h");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(330, 24));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(26, 13));
			((Control)label4).set_TabIndex(11);
			((Control)label4).set_Text("m.m");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(410, 24));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(13, 13));
			((Control)label3).set_TabIndex(14);
			((Control)label3).set_Text("o");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(60, 24));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(41, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Primary");
			((Control)txtRAH).set_Location(new Point(302, 39));
			((Control)txtRAH).set_Name("txtRAH");
			((Control)txtRAH).set_Size(new Size(20, 20));
			((Control)txtRAH).set_TabIndex(10);
			((Control)txtXZsecondary).set_Location(new Point(120, 39));
			((Control)txtXZsecondary).set_Name("txtXZsecondary");
			((Control)txtXZsecondary).set_Size(new Size(57, 20));
			((Control)txtXZsecondary).set_TabIndex(4);
			((Control)txtXZprinciple).set_Location(new Point(183, 39));
			((Control)txtXZprinciple).set_Name("txtXZprinciple");
			((Control)txtXZprinciple).set_Size(new Size(57, 20));
			((Control)txtXZprinciple).set_TabIndex(6);
			((Control)txtRAM).set_Location(new Point(327, 39));
			((Control)txtRAM).set_Name("txtRAM");
			((Control)txtRAM).set_Size(new Size(30, 20));
			((Control)txtRAM).set_TabIndex(12);
			((Control)txtDecD).set_Location(new Point(401, 39));
			((Control)txtDecD).set_Name("txtDecD");
			((Control)txtDecD).set_Size(new Size(23, 20));
			((Control)txtDecD).set_TabIndex(15);
			((Control)txtDecM).set_Location(new Point(428, 39));
			((Control)txtDecM).set_Name("txtDecM");
			((Control)txtDecM).set_Size(new Size(20, 20));
			((Control)txtDecM).set_TabIndex(17);
			((Control)txtCode).set_Location(new Point(518, 39));
			((Control)txtCode).set_Name("txtCode");
			((Control)txtCode).set_Size(new Size(31, 20));
			((Control)txtCode).set_TabIndex(20);
			((Control)txtCode).add_TextChanged((EventHandler)txtCode_TextChanged);
			((Control)txtPMDec).set_Location(new Point(784, 39));
			((Control)txtPMDec).set_Name("txtPMDec");
			((Control)txtPMDec).set_Size(new Size(40, 20));
			((Control)txtPMDec).set_TabIndex(29);
			((Control)txtPMRA).set_Location(new Point(738, 39));
			((Control)txtPMRA).set_Name("txtPMRA");
			((Control)txtPMRA).set_Size(new Size(40, 20));
			((Control)txtPMRA).set_TabIndex(27);
			((Control)txtPair).set_Location(new Point(606, 39));
			((Control)txtPair).set_Name("txtPair");
			((Control)txtPair).set_Size(new Size(57, 20));
			((Control)txtPair).set_TabIndex(24);
			((Control)txtNumber).set_Location(new Point(558, 39));
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(39, 20));
			((Control)txtNumber).set_TabIndex(22);
			((Control)txtXZprimary).set_Location(new Point(57, 39));
			((Control)txtXZprimary).set_Name("txtXZprimary");
			((Control)txtXZprimary).set_Size(new Size(57, 20));
			((Control)txtXZprimary).set_TabIndex(2);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(436, 29));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(9, 13));
			((Control)label2).set_TabIndex(16);
			((Control)label2).set_Text("'");
			((Control)lblCode).set_AutoSize(true);
			((Control)lblCode).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCode).set_Location(new Point(518, 59));
			((Control)lblCode).set_Name("lblCode");
			((Control)lblCode).set_Size(new Size(16, 13));
			((Control)lblCode).set_TabIndex(33);
			((Control)lblCode).set_Text("...");
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbSep2_LessThan);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbSep1_LessThan);
			((Control)groupBox2).get_Controls().Add((Control)(object)label32);
			((Control)groupBox2).get_Controls().Add((Control)(object)label31);
			((Control)groupBox2).get_Controls().Add((Control)(object)label30);
			((Control)groupBox2).get_Controls().Add((Control)(object)label29);
			((Control)groupBox2).get_Controls().Add((Control)(object)label28);
			((Control)groupBox2).get_Controls().Add((Control)(object)label27);
			((Control)groupBox2).get_Controls().Add((Control)(object)label26);
			((Control)groupBox2).get_Controls().Add((Control)(object)label25);
			((Control)groupBox2).get_Controls().Add((Control)(object)label22);
			((Control)groupBox2).get_Controls().Add((Control)(object)label20);
			((Control)groupBox2).get_Controls().Add((Control)(object)label19);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbWDS3);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbWDS2);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbWDS1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtY1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtY2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtPA1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtPA2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtSep1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtSep2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtNumObs);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMag1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMag2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtSpectrum);
			((Control)groupBox2).set_Location(new Point(2, 372));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(968, 91));
			((Control)groupBox2).set_TabIndex(2);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Double star details");
			cmbSep2_LessThan.set_DropDownStyle((ComboBoxStyle)2);
			cmbSep2_LessThan.set_DropDownWidth(11);
			((ListControl)cmbSep2_LessThan).set_FormattingEnabled(true);
			cmbSep2_LessThan.get_Items().AddRange(new object[2] { "", "<" });
			((Control)cmbSep2_LessThan).set_Location(new Point(227, 58));
			((Control)cmbSep2_LessThan).set_Name("cmbSep2_LessThan");
			((Control)cmbSep2_LessThan).set_Size(new Size(31, 21));
			((Control)cmbSep2_LessThan).set_TabIndex(25);
			cmbSep1_LessThan.set_DropDownStyle((ComboBoxStyle)2);
			cmbSep1_LessThan.set_DropDownWidth(11);
			((ListControl)cmbSep1_LessThan).set_FormattingEnabled(true);
			cmbSep1_LessThan.get_Items().AddRange(new object[2] { "", "<" });
			((Control)cmbSep1_LessThan).set_Location(new Point(227, 35));
			((Control)cmbSep1_LessThan).set_Name("cmbSep1_LessThan");
			((Control)cmbSep1_LessThan).set_Size(new Size(31, 21));
			((Control)cmbSep1_LessThan).set_TabIndex(24);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(117, 19));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(29, 13));
			((Control)label32).set_TabIndex(2);
			((Control)label32).set_Text("Year");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(175, 19));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(27, 13));
			((Control)label31).set_TabIndex(5);
			((Control)label31).set_Text("P.A.");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Location(new Point(255, 19));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(58, 13));
			((Control)label30).set_TabIndex(8);
			((Control)label30).set_Text("Separation");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(342, 44));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(67, 26));
			((Control)label29).set_TabIndex(11);
			((Control)label29).set_Text("Number of\r\nobservations");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(504, 39));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(41, 13));
			((Control)label28).set_TabIndex(13);
			((Control)label28).set_Text("Primary");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(14, 39));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(86, 13));
			((Control)label27).set_TabIndex(0);
			((Control)label27).set_Text("First Observation");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(14, 62));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(87, 13));
			((Control)label26).set_TabIndex(1);
			((Control)label26).set_Text("Last Observation");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(614, 19));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(69, 13));
			((Control)label25).set_TabIndex(18);
			((Control)label25).set_Text("Spectral type");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(810, 19));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(64, 13));
			((Control)label22).set_TabIndex(21);
			((Control)label22).set_Text("WDS Notes");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(552, 19));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(31, 13));
			((Control)label20).set_TabIndex(15);
			((Control)label20).set_Text("Mag.");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(487, 62));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(58, 13));
			((Control)label19).set_TabIndex(14);
			((Control)label19).set_Text("Secondary");
			cmbWDS3.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbWDS3).set_FormattingEnabled(true);
			cmbWDS3.get_Items().AddRange(new object[14]
			{
				" ", "D", "N", "O", "P", "Q", "R", "a", "p", "r",
				"s", "6", "X", "Z"
			});
			((Control)cmbWDS3).set_Location(new Point(870, 35));
			cmbWDS3.set_MaxDropDownItems(20);
			((Control)cmbWDS3).set_Name("cmbWDS3");
			((Control)cmbWDS3).set_Size(new Size(37, 21));
			((Control)cmbWDS3).set_TabIndex(23);
			cmbWDS2.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbWDS2).set_FormattingEnabled(true);
			cmbWDS2.get_Items().AddRange(new object[14]
			{
				" ", "D", "N", "O", "P", "Q", "R", "a", "p", "r",
				"s", "6", "X", "Z"
			});
			((Control)cmbWDS2).set_Location(new Point(827, 35));
			cmbWDS2.set_MaxDropDownItems(20);
			((Control)cmbWDS2).set_Name("cmbWDS2");
			((Control)cmbWDS2).set_Size(new Size(37, 21));
			((Control)cmbWDS2).set_TabIndex(22);
			cmbWDS1.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbWDS1).set_FormattingEnabled(true);
			cmbWDS1.get_Items().AddRange(new object[14]
			{
				" ", "D", "N", "O", "P", "Q", "R", "a", "p", "r",
				"s", "6", "X", "Z"
			});
			((Control)cmbWDS1).set_Location(new Point(784, 35));
			cmbWDS1.set_MaxDropDownItems(20);
			((Control)cmbWDS1).set_Name("cmbWDS1");
			((Control)cmbWDS1).set_Size(new Size(37, 21));
			((Control)cmbWDS1).set_TabIndex(20);
			((Control)txtY1).set_Location(new Point(114, 35));
			((Control)txtY1).set_Name("txtY1");
			((Control)txtY1).set_Size(new Size(37, 20));
			((Control)txtY1).set_TabIndex(3);
			((Control)txtY2).set_Location(new Point(114, 59));
			((Control)txtY2).set_Name("txtY2");
			((Control)txtY2).set_Size(new Size(37, 20));
			((Control)txtY2).set_TabIndex(4);
			((Control)txtPA1).set_Location(new Point(168, 35));
			((Control)txtPA1).set_Name("txtPA1");
			((Control)txtPA1).set_Size(new Size(48, 20));
			((Control)txtPA1).set_TabIndex(6);
			((Control)txtPA2).set_Location(new Point(168, 59));
			((Control)txtPA2).set_Name("txtPA2");
			((Control)txtPA2).set_Size(new Size(48, 20));
			((Control)txtPA2).set_TabIndex(7);
			((Control)txtSep1).set_Location(new Point(258, 35));
			((Control)txtSep1).set_Name("txtSep1");
			((Control)txtSep1).set_Size(new Size(57, 20));
			((Control)txtSep1).set_TabIndex(9);
			((Control)txtSep2).set_Location(new Point(258, 59));
			((Control)txtSep2).set_Name("txtSep2");
			((Control)txtSep2).set_Size(new Size(57, 20));
			((Control)txtSep2).set_TabIndex(10);
			((Control)txtNumObs).set_Location(new Point(410, 47));
			((Control)txtNumObs).set_Name("txtNumObs");
			((Control)txtNumObs).set_Size(new Size(37, 20));
			((Control)txtNumObs).set_TabIndex(12);
			((Control)txtMag1).set_Location(new Point(555, 35));
			((Control)txtMag1).set_Name("txtMag1");
			((Control)txtMag1).set_Size(new Size(34, 20));
			((Control)txtMag1).set_TabIndex(16);
			((Control)txtMag2).set_Location(new Point(555, 59));
			((Control)txtMag2).set_Name("txtMag2");
			((Control)txtMag2).set_Size(new Size(34, 20));
			((Control)txtMag2).set_TabIndex(17);
			((Control)txtSpectrum).set_Location(new Point(610, 35));
			((Control)txtSpectrum).set_Name("txtSpectrum");
			((Control)txtSpectrum).set_Size(new Size(80, 20));
			((Control)txtSpectrum).set_TabIndex(19);
			((Control)groupBox3).get_Controls().Add((Control)(object)label36);
			((Control)groupBox3).get_Controls().Add((Control)(object)label35);
			((Control)groupBox3).get_Controls().Add((Control)(object)label34);
			((Control)groupBox3).get_Controls().Add((Control)(object)label33);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtI);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtA);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtP);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtPeri);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtE);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtT);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtNode);
			((Control)groupBox3).get_Controls().Add((Control)(object)label21);
			((Control)groupBox3).get_Controls().Add((Control)(object)label23);
			((Control)groupBox3).get_Controls().Add((Control)(object)label24);
			((Control)groupBox3).set_Location(new Point(2, 464));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(968, 51));
			((Control)groupBox3).set_TabIndex(3);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Orbital elements");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Symbol", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 2));
			((Control)label36).set_Location(new Point(825, 26));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(16, 13));
			((Control)label36).set_TabIndex(12);
			((Control)label36).set_Text("v");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Location(new Point(697, 26));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(13, 13));
			((Control)label35).set_TabIndex(10);
			((Control)label35).set_Text("e");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Location(new Point(565, 26));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(14, 13));
			((Control)label34).set_TabIndex(8);
			((Control)label34).set_Text("T");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Symbol", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 2));
			((Control)label33).set_Location(new Point(432, 26));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(16, 13));
			((Control)label33).set_TabIndex(6);
			((Control)label33).set_Text("W");
			((Control)txtI).set_Location(new Point(319, 23));
			((Control)txtI).set_Name("txtI");
			((Control)txtI).set_Size(new Size(80, 20));
			((Control)txtI).set_TabIndex(5);
			((Control)txtA).set_Location(new Point(188, 23));
			((Control)txtA).set_Name("txtA");
			((Control)txtA).set_Size(new Size(80, 20));
			((Control)txtA).set_TabIndex(3);
			((Control)txtP).set_Location(new Point(57, 23));
			((Control)txtP).set_Name("txtP");
			((Control)txtP).set_Size(new Size(80, 20));
			((Control)txtP).set_TabIndex(1);
			((Control)txtPeri).set_Location(new Point(843, 23));
			((Control)txtPeri).set_Name("txtPeri");
			((Control)txtPeri).set_Size(new Size(80, 20));
			((Control)txtPeri).set_TabIndex(13);
			((Control)txtE).set_Location(new Point(712, 23));
			((Control)txtE).set_Name("txtE");
			((Control)txtE).set_Size(new Size(80, 20));
			((Control)txtE).set_TabIndex(11);
			((Control)txtT).set_Location(new Point(581, 23));
			((Control)txtT).set_Name("txtT");
			((Control)txtT).set_Size(new Size(80, 20));
			((Control)txtT).set_TabIndex(9);
			((Control)txtNode).set_Location(new Point(450, 23));
			((Control)txtNode).set_Name("txtNode");
			((Control)txtNode).set_Size(new Size(80, 20));
			((Control)txtNode).set_TabIndex(7);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(41, 26));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(14, 13));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("P");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(308, 26));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(9, 13));
			((Control)label23).set_TabIndex(4);
			((Control)label23).set_Text("i");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(173, 26));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(13, 13));
			((Control)label24).set_TabIndex(2);
			((Control)label24).set_Text("a");
			((Control)cmdFind).set_Location(new Point(38, 526));
			((Control)cmdFind).set_Name("cmdFind");
			((Control)cmdFind).set_Size(new Size(44, 31));
			((Control)cmdFind).set_TabIndex(4);
			((Control)cmdFind).set_Text("Find");
			((ButtonBase)cmdFind).set_UseVisualStyleBackColor(true);
			((Control)cmdFind).add_Click((EventHandler)cmdFind_Click);
			((Control)cmdAdd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAdd).set_Location(new Point(598, 521));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(75, 41));
			((Control)cmdAdd).set_TabIndex(14);
			((Control)cmdAdd).set_Text("Add as \r\nNew entry");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdReplace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReplace).set_Location(new Point(694, 521));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(65, 41));
			((Control)cmdReplace).set_TabIndex(15);
			((Control)cmdReplace).set_Text("Replace entry");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdDelete).set_Location(new Point(780, 521));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(65, 41));
			((Control)cmdDelete).set_TabIndex(16);
			((Control)cmdDelete).set_Text("Delete entry");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)txtXZSearch).set_Location(new Point(109, 532));
			((Control)txtXZSearch).set_Name("txtXZSearch");
			((Control)txtXZSearch).set_Size(new Size(57, 20));
			((Control)txtXZSearch).set_TabIndex(6);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Location(new Point(86, 535));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(21, 13));
			((Control)label39).set_TabIndex(5);
			((Control)label39).set_Text("XZ");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Location(new Point(257, 519));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(32, 13));
			((Control)label38).set_TabIndex(8);
			((Control)label38).set_Text("Code");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(295, 519));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(44, 13));
			((Control)label40).set_TabIndex(10);
			((Control)label40).set_Text("Number");
			((Control)txtIDSearchCode).set_Location(new Point(257, 534));
			((Control)txtIDSearchCode).set_Name("txtIDSearchCode");
			((Control)txtIDSearchCode).set_Size(new Size(31, 20));
			((Control)txtIDSearchCode).set_TabIndex(9);
			((Control)txtIDSearchNumber).set_Location(new Point(297, 534));
			((Control)txtIDSearchNumber).set_Name("txtIDSearchNumber");
			((Control)txtIDSearchNumber).set_Size(new Size(39, 20));
			((Control)txtIDSearchNumber).set_TabIndex(11);
			((Control)cmdFindID).set_Location(new Point(207, 526));
			((Control)cmdFindID).set_Name("cmdFindID");
			((Control)cmdFindID).set_Size(new Size(44, 31));
			((Control)cmdFindID).set_TabIndex(7);
			((Control)cmdFindID).set_Text("Find");
			((ButtonBase)cmdFindID).set_UseVisualStyleBackColor(true);
			((Control)cmdFindID).add_Click((EventHandler)cmdFindID_Click);
			((Control)panelDoubles).set_Anchor((AnchorStyles)1);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdRetrieveWDS);
			((Control)panelDoubles).get_Controls().Add((Control)(object)groupBox8);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdSortXZDoubles);
			((Control)panelDoubles).get_Controls().Add((Control)(object)lstDoubles);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdDoublesSearchInterferometer);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdFindID);
			((Control)panelDoubles).get_Controls().Add((Control)(object)label38);
			((Control)panelDoubles).get_Controls().Add((Control)(object)label40);
			((Control)panelDoubles).get_Controls().Add((Control)(object)txtIDSearchCode);
			((Control)panelDoubles).get_Controls().Add((Control)(object)txtIDSearchNumber);
			((Control)panelDoubles).get_Controls().Add((Control)(object)label39);
			((Control)panelDoubles).get_Controls().Add((Control)(object)txtXZSearch);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdDelete);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdReplace);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdAdd);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdFind);
			((Control)panelDoubles).get_Controls().Add((Control)(object)groupBox3);
			((Control)panelDoubles).get_Controls().Add((Control)(object)groupBox2);
			((Control)panelDoubles).get_Controls().Add((Control)(object)groupBox1);
			((Control)panelDoubles).set_Location(new Point(4, 27));
			((Control)panelDoubles).set_Name("panelDoubles");
			((Control)panelDoubles).set_Size(new Size(1001, 637));
			((Control)panelDoubles).set_TabIndex(28);
			((Control)cmdRetrieveWDS).set_Location(new Point(360, 521));
			((Control)cmdRetrieveWDS).set_Name("cmdRetrieveWDS");
			((Control)cmdRetrieveWDS).set_Size(new Size(65, 41));
			((Control)cmdRetrieveWDS).set_TabIndex(12);
			((Control)cmdRetrieveWDS).set_Text("Retrieve WDS data");
			((ButtonBase)cmdRetrieveWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveWDS).add_Click((EventHandler)cmdRetrieveWDS_Click);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdAddToOCC);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdGoToNextAdded);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdEditOCC);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdFindWDSMissingFromXZDoubles);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdCancel);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdWDSUpdateALL);
			((Control)groupBox8).set_Location(new Point(2, 577));
			((Control)groupBox8).set_Name("groupBox8");
			((Control)groupBox8).set_Size(new Size(968, 53));
			((Control)groupBox8).set_TabIndex(18);
			groupBox8.set_TabStop(false);
			((Control)groupBox8).set_Text("Update data in XZDoubles");
			((Control)cmdAddToOCC).set_Location(new Point(606, 10));
			((Control)cmdAddToOCC).set_Name("cmdAddToOCC");
			((Control)cmdAddToOCC).set_Size(new Size(65, 13));
			((Control)cmdAddToOCC).set_TabIndex(6);
			((Control)cmdAddToOCC).set_Text("AddToOcc");
			((ButtonBase)cmdAddToOCC).set_UseVisualStyleBackColor(true);
			((Control)cmdAddToOCC).set_Visible(false);
			((Control)cmdAddToOCC).add_Click((EventHandler)cmdAddToOCC_Click);
			((Control)cmdGoToNextAdded).set_Location(new Point(461, 20));
			((Control)cmdGoToNextAdded).set_Name("cmdGoToNextAdded");
			((Control)cmdGoToNextAdded).set_Size(new Size(53, 21));
			((Control)cmdGoToNextAdded).set_TabIndex(3);
			((Control)cmdGoToNextAdded).set_Text("Next #");
			((ButtonBase)cmdGoToNextAdded).set_UseVisualStyleBackColor(true);
			((Control)cmdGoToNextAdded).add_Click((EventHandler)cmdGoToNextAdded_Click);
			((Control)cmdEditOCC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdEditOCC).set_Location(new Point(763, 9));
			((Control)cmdEditOCC).set_Name("cmdEditOCC");
			((Control)cmdEditOCC).set_Size(new Size(78, 41));
			((Control)cmdEditOCC).set_TabIndex(5);
			((Control)cmdEditOCC).set_Text("OCC stars\r\nEditor");
			((ButtonBase)cmdEditOCC).set_UseVisualStyleBackColor(true);
			((Control)cmdEditOCC).add_Click((EventHandler)cmdEditOCC_Click);
			((Control)cmdFindWDSMissingFromXZDoubles).set_Location(new Point(326, 17));
			((Control)cmdFindWDSMissingFromXZDoubles).set_Name("cmdFindWDSMissingFromXZDoubles");
			((Control)cmdFindWDSMissingFromXZDoubles).set_Size(new Size(121, 26));
			((Control)cmdFindWDSMissingFromXZDoubles).set_TabIndex(2);
			((Control)cmdFindWDSMissingFromXZDoubles).set_Text("Add new WDS entries");
			((ButtonBase)cmdFindWDSMissingFromXZDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdFindWDSMissingFromXZDoubles).add_Click((EventHandler)cmdFindWDSMissingFromXZDoubles_Click);
			((Control)cmdCancel).set_Location(new Point(177, 20));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(49, 20));
			((Control)cmdCancel).set_TabIndex(1);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdWDSUpdateALL).set_Location(new Point(33, 15));
			((Control)cmdWDSUpdateALL).set_Name("cmdWDSUpdateALL");
			((Control)cmdWDSUpdateALL).set_Size(new Size(141, 30));
			((Control)cmdWDSUpdateALL).set_TabIndex(0);
			((Control)cmdWDSUpdateALL).set_Text("Update with current WDS");
			((ButtonBase)cmdWDSUpdateALL).set_UseVisualStyleBackColor(true);
			((Control)cmdWDSUpdateALL).add_Click((EventHandler)cmdWDSUpdateALL_Click);
			((Control)cmdSortXZDoubles).set_Location(new Point(467, 521));
			((Control)cmdSortXZDoubles).set_Name("cmdSortXZDoubles");
			((Control)cmdSortXZDoubles).set_Size(new Size(65, 41));
			((Control)cmdSortXZDoubles).set_TabIndex(13);
			((Control)cmdSortXZDoubles).set_Text("Sort list");
			((ButtonBase)cmdSortXZDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdSortXZDoubles).add_Click((EventHandler)cmdSortXZDoubles_Click);
			((Control)cmdDoublesSearchInterferometer).set_Location(new Point(872, 524));
			((Control)cmdDoublesSearchInterferometer).set_Name("cmdDoublesSearchInterferometer");
			((Control)cmdDoublesSearchInterferometer).set_Size(new Size(110, 34));
			((Control)cmdDoublesSearchInterferometer).set_TabIndex(17);
			((Control)cmdDoublesSearchInterferometer).set_Text("Display WDS && Interferometric Cats");
			((ButtonBase)cmdDoublesSearchInterferometer).set_UseVisualStyleBackColor(true);
			((Control)cmdDoublesSearchInterferometer).add_Click((EventHandler)cmdDoublesSearchInterferometer_Click);
			((Control)panelOCC).set_Anchor((AnchorStyles)1);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmbLightCurve);
			((Control)panelOCC).get_Controls().Add((Control)(object)label81);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmdCompareAllOCCwithIF);
			((Control)panelOCC).get_Controls().Add((Control)(object)button1);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmbDoubleStatus);
			((Control)panelOCC).get_Controls().Add((Control)(object)label80);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmdReplaceOCC);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmdAddOCC);
			((Control)panelOCC).get_Controls().Add((Control)(object)groupBox7);
			((Control)panelOCC).get_Controls().Add((Control)(object)groupBox6);
			((Control)panelOCC).get_Controls().Add((Control)(object)groupBox5);
			((Control)panelOCC).get_Controls().Add((Control)(object)lstNames);
			((Control)panelOCC).get_Controls().Add((Control)(object)groupBox4);
			((Control)panelOCC).get_Controls().Add((Control)(object)lstOCC);
			((Control)panelOCC).get_Controls().Add((Control)(object)cmdToDoubles);
			((Control)panelOCC).set_Location(new Point(0, 73));
			((Control)panelOCC).set_Name("panelOCC");
			((Control)panelOCC).set_Size(new Size(998, 634));
			((Control)panelOCC).set_TabIndex(29);
			((ListControl)cmbLightCurve).set_FormattingEnabled(true);
			cmbLightCurve.get_Items().AddRange(new object[3] { "No light curve available ", "Light-curve arguable or inconclusive", "Light curve definitely shows a double star" });
			((Control)cmbLightCurve).set_Location(new Point(255, 555));
			((Control)cmbLightCurve).set_Name("cmbLightCurve");
			((Control)cmbLightCurve).set_Size(new Size(266, 21));
			((Control)cmbLightCurve).set_TabIndex(36);
			((Control)label81).set_AutoSize(true);
			((Control)label81).set_Location(new Point(291, 536));
			((Control)label81).set_Name("label81");
			((Control)label81).set_Size(new Size(107, 13));
			((Control)label81).set_TabIndex(35);
			((Control)label81).set_Text("Light-curve evidence");
			label81.set_TextAlign(ContentAlignment.TopRight);
			((Control)cmdCompareAllOCCwithIF).set_Location(new Point(299, 587));
			((Control)cmdCompareAllOCCwithIF).set_Name("cmdCompareAllOCCwithIF");
			((Control)cmdCompareAllOCCwithIF).set_Size(new Size(178, 40));
			((Control)cmdCompareAllOCCwithIF).set_TabIndex(34);
			((Control)cmdCompareAllOCCwithIF).set_Text("List status mis-matches with\r\nthe Interferometric catalogue");
			((ButtonBase)cmdCompareAllOCCwithIF).set_UseVisualStyleBackColor(true);
			((Control)cmdCompareAllOCCwithIF).add_Click((EventHandler)cmdCompareAllOCCwithIF_Click);
			((Control)button1).set_Location(new Point(691, 496));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(172, 40));
			((Control)button1).set_TabIndex(33);
			((Control)button1).set_Text("Current entry - show WDS and\r\n Interferometric Catalog data");
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)button1_Click);
			((ListControl)cmbDoubleStatus).set_FormattingEnabled(true);
			cmbDoubleStatus.get_Items().AddRange(new object[3] { "A - Definite double -  listed in WDS or Interferometric catalog", "P - Possible double", "E - Definitely not a double" });
			((Control)cmbDoubleStatus).set_Location(new Point(254, 501));
			((Control)cmbDoubleStatus).set_Name("cmbDoubleStatus");
			((Control)cmbDoubleStatus).set_Size(new Size(269, 21));
			((Control)cmbDoubleStatus).set_TabIndex(10);
			((Control)label80).set_AutoSize(true);
			((Control)label80).set_Location(new Point(291, 483));
			((Control)label80).set_Name("label80");
			((Control)label80).set_Size(new Size(92, 13));
			((Control)label80).set_TabIndex(9);
			((Control)label80).set_Text("Double star status");
			label80.set_TextAlign(ContentAlignment.TopRight);
			((Control)cmdReplaceOCC).set_Location(new Point(741, 565));
			((Control)cmdReplaceOCC).set_Name("cmdReplaceOCC");
			((Control)cmdReplaceOCC).set_Size(new Size(96, 49));
			((Control)cmdReplaceOCC).set_TabIndex(7);
			((Control)cmdReplaceOCC).set_Text("Replace\r\ncurrent OCC");
			((ButtonBase)cmdReplaceOCC).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceOCC).add_Click((EventHandler)cmdReplaceOCC_Click);
			((Control)cmdAddOCC).set_Location(new Point(596, 565));
			((Control)cmdAddOCC).set_Name("cmdAddOCC");
			((Control)cmdAddOCC).set_Size(new Size(96, 49));
			((Control)cmdAddOCC).set_TabIndex(6);
			((Control)cmdAddOCC).set_Text("Add as \r\nnew OCC");
			((ButtonBase)cmdAddOCC).set_UseVisualStyleBackColor(true);
			((Control)cmdAddOCC).add_Click((EventHandler)cmdAddOCC_Click);
			((Control)groupBox7).get_Controls().Add((Control)(object)cmdOCCinit);
			((Control)groupBox7).get_Controls().Add((Control)(object)label79);
			((Control)groupBox7).get_Controls().Add((Control)(object)label78);
			((Control)groupBox7).get_Controls().Add((Control)(object)label77);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtXZinit);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtSAOinit);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtZCinit);
			((Control)groupBox7).set_Location(new Point(8, 473));
			((Control)groupBox7).set_Name("groupBox7");
			((Control)groupBox7).set_Size(new Size(240, 87));
			((Control)groupBox7).set_TabIndex(5);
			groupBox7.set_TabStop(false);
			((Control)groupBox7).set_Text("Initialise new OCC entry");
			((Control)cmdOCCinit).set_Location(new Point(154, 23));
			((Control)cmdOCCinit).set_Name("cmdOCCinit");
			((Control)cmdOCCinit).set_Size(new Size(66, 49));
			((Control)cmdOCCinit).set_TabIndex(6);
			((Control)cmdOCCinit).set_Text("Initialise\r\nnew \r\nOCC entry");
			((ButtonBase)cmdOCCinit).set_UseVisualStyleBackColor(true);
			((Control)cmdOCCinit).add_Click((EventHandler)cmdOCCinit_Click);
			((Control)label79).set_AutoSize(true);
			((Control)label79).set_Location(new Point(34, 19));
			((Control)label79).set_Name("label79");
			((Control)label79).set_Size(new Size(21, 13));
			((Control)label79).set_TabIndex(0);
			((Control)label79).set_Text("XZ");
			((Control)label78).set_AutoSize(true);
			((Control)label78).set_Location(new Point(26, 42));
			((Control)label78).set_Name("label78");
			((Control)label78).set_Size(new Size(29, 13));
			((Control)label78).set_TabIndex(2);
			((Control)label78).set_Text("SAO");
			((Control)label77).set_AutoSize(true);
			((Control)label77).set_Location(new Point(34, 62));
			((Control)label77).set_Name("label77");
			((Control)label77).set_Size(new Size(21, 13));
			((Control)label77).set_TabIndex(4);
			((Control)label77).set_Text("ZC");
			((Control)txtXZinit).set_Location(new Point(60, 16));
			((Control)txtXZinit).set_Name("txtXZinit");
			((Control)txtXZinit).set_Size(new Size(57, 20));
			((Control)txtXZinit).set_TabIndex(1);
			((Control)txtXZinit).add_TextChanged((EventHandler)txtXZinit_TextChanged);
			((Control)txtSAOinit).set_Location(new Point(60, 39));
			((Control)txtSAOinit).set_Name("txtSAOinit");
			((Control)txtSAOinit).set_Size(new Size(57, 20));
			((Control)txtSAOinit).set_TabIndex(3);
			((Control)txtSAOinit).add_TextChanged((EventHandler)txtSAOinit_TextChanged);
			((Control)txtZCinit).set_Location(new Point(60, 62));
			((Control)txtZCinit).set_Name("txtZCinit");
			((Control)txtZCinit).set_Size(new Size(42, 20));
			((Control)txtZCinit).set_TabIndex(5);
			((Control)txtZCinit).add_TextChanged((EventHandler)txtZCinit_TextChanged);
			((Control)groupBox6).get_Controls().Add((Control)(object)label76);
			((Control)groupBox6).get_Controls().Add((Control)(object)label75);
			((Control)groupBox6).get_Controls().Add((Control)(object)label74);
			((Control)groupBox6).get_Controls().Add((Control)(object)label73);
			((Control)groupBox6).get_Controls().Add((Control)(object)label70);
			((Control)groupBox6).get_Controls().Add((Control)(object)label71);
			((Control)groupBox6).get_Controls().Add((Control)(object)label72);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbD4);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbM4);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnY4);
			((Control)groupBox6).get_Controls().Add((Control)(object)label67);
			((Control)groupBox6).get_Controls().Add((Control)(object)label68);
			((Control)groupBox6).get_Controls().Add((Control)(object)label69);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbD3);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbM3);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnY3);
			((Control)groupBox6).get_Controls().Add((Control)(object)label64);
			((Control)groupBox6).get_Controls().Add((Control)(object)label65);
			((Control)groupBox6).get_Controls().Add((Control)(object)label66);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbD5);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbM5);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnY5);
			((Control)groupBox6).get_Controls().Add((Control)(object)label61);
			((Control)groupBox6).get_Controls().Add((Control)(object)label62);
			((Control)groupBox6).get_Controls().Add((Control)(object)label63);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbD2);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbM2);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnY2);
			((Control)groupBox6).set_Location(new Point(8, 416));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(983, 56));
			((Control)groupBox6).set_TabIndex(4);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("Confirmation dates");
			((Control)label76).set_AutoSize(true);
			((Control)label76).set_Location(new Point(49, 33));
			((Control)label76).set_Name("label76");
			((Control)label76).set_Size(new Size(26, 13));
			((Control)label76).set_TabIndex(0);
			((Control)label76).set_Text("First");
			label76.set_TextAlign(ContentAlignment.TopRight);
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_Location(new Point(278, 33));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(44, 13));
			((Control)label75).set_TabIndex(7);
			((Control)label75).set_Text("Second");
			label75.set_TextAlign(ContentAlignment.TopRight);
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_Location(new Point(777, 33));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(37, 13));
			((Control)label74).set_TabIndex(21);
			((Control)label74).set_Text("Fourth");
			label74.set_TextAlign(ContentAlignment.TopRight);
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Location(new Point(537, 33));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(31, 13));
			((Control)label73).set_TabIndex(14);
			((Control)label73).set_Text("Third");
			label73.set_TextAlign(ContentAlignment.TopRight);
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_Location(new Point(662, 16));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(26, 13));
			((Control)label70).set_TabIndex(19);
			((Control)label70).set_Text("Day");
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_Location(new Point(617, 16));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(37, 13));
			((Control)label71).set_TabIndex(17);
			((Control)label71).set_Text("Month");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Location(new Point(571, 16));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(29, 13));
			((Control)label72).set_TabIndex(15);
			((Control)label72).set_Text("Year");
			cmbD4.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbD4).set_FormattingEnabled(true);
			cmbD4.get_Items().AddRange(new object[32]
			{
				"", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbD4).set_Location(new Point(664, 29));
			cmbD4.set_MaxDropDownItems(31);
			((Control)cmbD4).set_Name("cmbD4");
			((Control)cmbD4).set_Size(new Size(40, 21));
			((Control)cmbD4).set_TabIndex(20);
			cmbM4.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbM4).set_FormattingEnabled(true);
			cmbM4.get_Items().AddRange(new object[13]
			{
				"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec"
			});
			((Control)cmbM4).set_Location(new Point(621, 29));
			cmbM4.set_MaxDropDownItems(12);
			((Control)cmbM4).set_Name("cmbM4");
			((Control)cmbM4).set_Size(new Size(43, 21));
			((Control)cmbM4).set_TabIndex(18);
			((Control)updnY4).set_Location(new Point(570, 30));
			updnY4.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnY4).set_Name("updnY4");
			((Control)updnY4).set_Size(new Size(51, 20));
			((Control)updnY4).set_TabIndex(16);
			updnY4.set_Value(new decimal(new int[4] { 2005, 0, 0, 0 }));
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Location(new Point(416, 16));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(26, 13));
			((Control)label67).set_TabIndex(12);
			((Control)label67).set_Text("Day");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Location(new Point(371, 16));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(37, 13));
			((Control)label68).set_TabIndex(10);
			((Control)label68).set_Text("Month");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Location(new Point(325, 16));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(29, 13));
			((Control)label69).set_TabIndex(8);
			((Control)label69).set_Text("Year");
			cmbD3.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbD3).set_FormattingEnabled(true);
			cmbD3.get_Items().AddRange(new object[32]
			{
				"", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbD3).set_Location(new Point(418, 29));
			cmbD3.set_MaxDropDownItems(31);
			((Control)cmbD3).set_Name("cmbD3");
			((Control)cmbD3).set_Size(new Size(40, 21));
			((Control)cmbD3).set_TabIndex(13);
			cmbM3.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbM3).set_FormattingEnabled(true);
			cmbM3.get_Items().AddRange(new object[13]
			{
				"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec"
			});
			((Control)cmbM3).set_Location(new Point(375, 29));
			cmbM3.set_MaxDropDownItems(12);
			((Control)cmbM3).set_Name("cmbM3");
			((Control)cmbM3).set_Size(new Size(43, 21));
			((Control)cmbM3).set_TabIndex(11);
			((Control)updnY3).set_Location(new Point(324, 30));
			updnY3.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnY3).set_Name("updnY3");
			((Control)updnY3).set_Size(new Size(51, 20));
			((Control)updnY3).set_TabIndex(9);
			updnY3.set_Value(new decimal(new int[4] { 2005, 0, 0, 0 }));
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Location(new Point(909, 16));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(26, 13));
			((Control)label64).set_TabIndex(26);
			((Control)label64).set_Text("Day");
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Location(new Point(864, 16));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(37, 13));
			((Control)label65).set_TabIndex(24);
			((Control)label65).set_Text("Month");
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Location(new Point(818, 16));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(29, 13));
			((Control)label66).set_TabIndex(22);
			((Control)label66).set_Text("Year");
			cmbD5.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbD5).set_FormattingEnabled(true);
			cmbD5.get_Items().AddRange(new object[32]
			{
				"", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbD5).set_Location(new Point(910, 29));
			cmbD5.set_MaxDropDownItems(31);
			((Control)cmbD5).set_Name("cmbD5");
			((Control)cmbD5).set_Size(new Size(40, 21));
			((Control)cmbD5).set_TabIndex(27);
			cmbM5.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbM5).set_FormattingEnabled(true);
			cmbM5.get_Items().AddRange(new object[13]
			{
				"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec"
			});
			((Control)cmbM5).set_Location(new Point(867, 29));
			cmbM5.set_MaxDropDownItems(12);
			((Control)cmbM5).set_Name("cmbM5");
			((Control)cmbM5).set_Size(new Size(43, 21));
			((Control)cmbM5).set_TabIndex(25);
			((Control)updnY5).set_Location(new Point(816, 30));
			updnY5.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnY5).set_Name("updnY5");
			((Control)updnY5).set_Size(new Size(51, 20));
			((Control)updnY5).set_TabIndex(23);
			updnY5.set_Value(new decimal(new int[4] { 2005, 0, 0, 0 }));
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Location(new Point(170, 16));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(26, 13));
			((Control)label61).set_TabIndex(5);
			((Control)label61).set_Text("Day");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Location(new Point(125, 16));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(37, 13));
			((Control)label62).set_TabIndex(3);
			((Control)label62).set_Text("Month");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Location(new Point(79, 16));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(29, 13));
			((Control)label63).set_TabIndex(1);
			((Control)label63).set_Text("Year");
			cmbD2.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbD2).set_FormattingEnabled(true);
			cmbD2.get_Items().AddRange(new object[32]
			{
				"", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbD2).set_Location(new Point(172, 29));
			cmbD2.set_MaxDropDownItems(31);
			((Control)cmbD2).set_Name("cmbD2");
			((Control)cmbD2).set_Size(new Size(40, 21));
			((Control)cmbD2).set_TabIndex(6);
			cmbM2.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbM2).set_FormattingEnabled(true);
			cmbM2.get_Items().AddRange(new object[13]
			{
				"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec"
			});
			((Control)cmbM2).set_Location(new Point(129, 29));
			cmbM2.set_MaxDropDownItems(12);
			((Control)cmbM2).set_Name("cmbM2");
			((Control)cmbM2).set_Size(new Size(43, 21));
			((Control)cmbM2).set_TabIndex(4);
			((Control)updnY2).set_Location(new Point(78, 30));
			updnY2.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnY2).set_Name("updnY2");
			((Control)updnY2).set_Size(new Size(51, 20));
			((Control)updnY2).set_TabIndex(2);
			updnY2.set_Value(new decimal(new int[4] { 2005, 0, 0, 0 }));
			((Control)groupBox5).get_Controls().Add((Control)(object)label51);
			((Control)groupBox5).get_Controls().Add((Control)(object)label58);
			((Control)groupBox5).get_Controls().Add((Control)(object)label60);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmbD1);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmbM1);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnY1);
			((Control)groupBox5).get_Controls().Add((Control)(object)label45);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtReference);
			((Control)groupBox5).get_Controls().Add((Control)(object)label43);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdNameFromList);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDiscoverer);
			((Control)groupBox5).set_Location(new Point(8, 360));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(983, 55));
			((Control)groupBox5).set_TabIndex(3);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Discovery details");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Location(new Point(573, 10));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(26, 13));
			((Control)label51).set_TabIndex(7);
			((Control)label51).set_Text("Day");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Location(new Point(528, 10));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(37, 13));
			((Control)label58).set_TabIndex(5);
			((Control)label58).set_Text("Month");
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Location(new Point(482, 10));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(29, 13));
			((Control)label60).set_TabIndex(3);
			((Control)label60).set_Text("Year");
			((ListControl)cmbD1).set_FormattingEnabled(true);
			cmbD1.get_Items().AddRange(new object[32]
			{
				"", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbD1).set_Location(new Point(575, 23));
			cmbD1.set_MaxDropDownItems(31);
			((Control)cmbD1).set_Name("cmbD1");
			((Control)cmbD1).set_Size(new Size(40, 21));
			((Control)cmbD1).set_TabIndex(8);
			((ListControl)cmbM1).set_FormattingEnabled(true);
			cmbM1.get_Items().AddRange(new object[13]
			{
				"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec"
			});
			((Control)cmbM1).set_Location(new Point(532, 23));
			cmbM1.set_MaxDropDownItems(12);
			((Control)cmbM1).set_Name("cmbM1");
			((Control)cmbM1).set_Size(new Size(43, 21));
			((Control)cmbM1).set_TabIndex(6);
			((Control)updnY1).set_Location(new Point(481, 24));
			updnY1.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnY1).set_Name("updnY1");
			((Control)updnY1).set_Size(new Size(51, 20));
			((Control)updnY1).set_TabIndex(4);
			updnY1.set_Value(new decimal(new int[4] { 2005, 0, 0, 0 }));
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Location(new Point(690, 30));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(74, 13));
			((Control)label45).set_TabIndex(9);
			((Control)label45).set_Text("O.N reference");
			label45.set_TextAlign(ContentAlignment.TopRight);
			((Control)txtReference).set_Location(new Point(768, 26));
			((Control)txtReference).set_Name("txtReference");
			((Control)txtReference).set_Size(new Size(162, 20));
			((Control)txtReference).set_TabIndex(10);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(26, 29));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(58, 13));
			((Control)label43).set_TabIndex(0);
			((Control)label43).set_Text("Discoverer");
			label43.set_TextAlign(ContentAlignment.TopRight);
			((Control)cmdNameFromList).set_Location(new Point(284, 14));
			((Control)cmdNameFromList).set_Name("cmdNameFromList");
			((Control)cmdNameFromList).set_Size(new Size(67, 36));
			((Control)cmdNameFromList).set_TabIndex(2);
			((Control)cmdNameFromList).set_Text("Pick name\r\nfrom list");
			((ButtonBase)cmdNameFromList).set_UseVisualStyleBackColor(true);
			((Control)cmdNameFromList).add_Click((EventHandler)cmdNameFromList_Click);
			((Control)txtDiscoverer).set_Location(new Point(104, 26));
			((Control)txtDiscoverer).set_Name("txtDiscoverer");
			((Control)txtDiscoverer).set_Size(new Size(162, 20));
			((Control)txtDiscoverer).set_TabIndex(1);
			lstNames.set_ColumnWidth(200);
			((Control)lstNames).set_Font(new Font("Courier New", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstNames).set_FormattingEnabled(true);
			lstNames.set_HorizontalScrollbar(true);
			lstNames.set_ItemHeight(12);
			((Control)lstNames).set_Location(new Point(4, 15));
			lstNames.set_MultiColumn(true);
			((Control)lstNames).set_Name("lstNames");
			((Control)lstNames).set_Size(new Size(991, 292));
			lstNames.set_Sorted(true);
			((Control)lstNames).set_TabIndex(1);
			((Control)lstNames).add_MouseDoubleClick(new MouseEventHandler(lstNames_MouseDoubleClick));
			((Control)groupBox4).get_Controls().Add((Control)(object)label41);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCBDNumber);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCBDZone);
			((Control)groupBox4).get_Controls().Add((Control)(object)label42);
			((Control)groupBox4).get_Controls().Add((Control)(object)label44);
			((Control)groupBox4).get_Controls().Add((Control)(object)label46);
			((Control)groupBox4).get_Controls().Add((Control)(object)label47);
			((Control)groupBox4).get_Controls().Add((Control)(object)label48);
			((Control)groupBox4).get_Controls().Add((Control)(object)label49);
			((Control)groupBox4).get_Controls().Add((Control)(object)label50);
			((Control)groupBox4).get_Controls().Add((Control)(object)label52);
			((Control)groupBox4).get_Controls().Add((Control)(object)label53);
			((Control)groupBox4).get_Controls().Add((Control)(object)label54);
			((Control)groupBox4).get_Controls().Add((Control)(object)label55);
			((Control)groupBox4).get_Controls().Add((Control)(object)label56);
			((Control)groupBox4).get_Controls().Add((Control)(object)label57);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCRAH);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCXZ);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCRAM);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCDecD);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCDecM);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCCode);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCRef);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCPair);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCCNumber);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtOCC);
			((Control)groupBox4).get_Controls().Add((Control)(object)label59);
			((Control)groupBox4).set_Location(new Point(8, 303));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(983, 56));
			((Control)groupBox4).set_TabIndex(2);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Star identification details");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Location(new Point(727, 13));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(81, 13));
			((Control)label41).set_TabIndex(22);
			((Control)label41).set_Text("BD / CD / CPD");
			((Control)txtOCCBDNumber).set_Location(new Point(757, 29));
			((Control)txtOCCBDNumber).set_Name("txtOCCBDNumber");
			((Control)txtOCCBDNumber).set_Size(new Size(55, 20));
			((Control)txtOCCBDNumber).set_TabIndex(24);
			((Control)txtOCCBDZone).set_Location(new Point(722, 29));
			((Control)txtOCCBDZone).set_Name("txtOCCBDZone");
			((Control)txtOCCBDZone).set_Size(new Size(28, 20));
			((Control)txtOCCBDZone).set_TabIndex(23);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(236, 24));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(65, 26));
			((Control)label42).set_TabIndex(4);
			((Control)label42).set_Text("Star position\r\n   [J2000]");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(840, 32));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(57, 13));
			((Control)label44).set_TabIndex(25);
			((Control)label44).set_Text("dsFile Ref.");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Location(new Point(485, 24));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(73, 26));
			((Control)label46).set_TabIndex(15);
			((Control)label46).set_Text("WDS Double-\r\nstar identifier");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Location(new Point(26, 32));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(29, 13));
			((Control)label47).set_TabIndex(0);
			((Control)label47).set_Text("OCC");
			label47.set_TextAlign(ContentAlignment.TopRight);
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Location(new Point(299, 32));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(22, 13));
			((Control)label48).set_TabIndex(5);
			((Control)label48).set_Text("RA");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Location(new Point(382, 32));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(27, 13));
			((Control)label49).set_TabIndex(10);
			((Control)label49).set_Text("Dec");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Location(new Point(560, 14));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(32, 13));
			((Control)label50).set_TabIndex(16);
			((Control)label50).set_Text("Code");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Location(new Point(126, 32));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(21, 13));
			((Control)label52).set_TabIndex(2);
			((Control)label52).set_Text("XZ");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Location(new Point(591, 14));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(44, 13));
			((Control)label53).set_TabIndex(18);
			((Control)label53).set_Text("Number");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Location(new Point(634, 14));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(25, 13));
			((Control)label54).set_TabIndex(20);
			((Control)label54).set_Text("Pair");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Location(new Point(326, 14));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(13, 13));
			((Control)label55).set_TabIndex(6);
			((Control)label55).set_Text("h");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Location(new Point(348, 14));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(26, 13));
			((Control)label56).set_TabIndex(8);
			((Control)label56).set_Text("m.m");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Location(new Point(420, 14));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(13, 13));
			((Control)label57).set_TabIndex(11);
			((Control)label57).set_Text("o");
			((Control)txtOCCRAH).set_Enabled(false);
			((Control)txtOCCRAH).set_Location(new Point(323, 29));
			((Control)txtOCCRAH).set_Name("txtOCCRAH");
			((Control)txtOCCRAH).set_Size(new Size(20, 20));
			((Control)txtOCCRAH).set_TabIndex(7);
			((Control)txtOCCXZ).set_Location(new Point(149, 29));
			((Control)txtOCCXZ).set_Name("txtOCCXZ");
			((Control)txtOCCXZ).set_Size(new Size(57, 20));
			((Control)txtOCCXZ).set_TabIndex(3);
			((Control)txtOCCRAM).set_Enabled(false);
			((Control)txtOCCRAM).set_Location(new Point(345, 29));
			((Control)txtOCCRAM).set_Name("txtOCCRAM");
			((Control)txtOCCRAM).set_Size(new Size(30, 20));
			((Control)txtOCCRAM).set_TabIndex(9);
			((Control)txtOCCDecD).set_Enabled(false);
			((Control)txtOCCDecD).set_Location(new Point(411, 29));
			((Control)txtOCCDecD).set_Name("txtOCCDecD");
			((Control)txtOCCDecD).set_Size(new Size(23, 20));
			((Control)txtOCCDecD).set_TabIndex(12);
			((Control)txtOCCDecM).set_Enabled(false);
			((Control)txtOCCDecM).set_Location(new Point(436, 29));
			((Control)txtOCCDecM).set_Name("txtOCCDecM");
			((Control)txtOCCDecM).set_Size(new Size(20, 20));
			((Control)txtOCCDecM).set_TabIndex(14);
			((Control)txtOCCCode).set_Location(new Point(560, 29));
			((Control)txtOCCCode).set_Name("txtOCCCode");
			((Control)txtOCCCode).set_Size(new Size(31, 20));
			((Control)txtOCCCode).set_TabIndex(17);
			((Control)txtOCCRef).set_Location(new Point(898, 29));
			((Control)txtOCCRef).set_Name("txtOCCRef");
			((Control)txtOCCRef).set_Size(new Size(58, 20));
			((Control)txtOCCRef).set_TabIndex(26);
			((Control)txtOCCPair).set_Location(new Point(634, 29));
			((Control)txtOCCPair).set_Name("txtOCCPair");
			((Control)txtOCCPair).set_Size(new Size(57, 20));
			((Control)txtOCCPair).set_TabIndex(21);
			((Control)txtOCCNumber).set_Location(new Point(593, 29));
			((Control)txtOCCNumber).set_Name("txtOCCNumber");
			((Control)txtOCCNumber).set_Size(new Size(39, 20));
			((Control)txtOCCNumber).set_TabIndex(19);
			((Control)txtOCC).set_Enabled(false);
			((Control)txtOCC).set_Location(new Point(57, 29));
			((Control)txtOCC).set_Name("txtOCC");
			((Control)txtOCC).set_Size(new Size(57, 20));
			((Control)txtOCC).set_TabIndex(1);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Location(new Point(444, 19));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(9, 13));
			((Control)label59).set_TabIndex(13);
			((Control)label59).set_Text("'");
			((Control)lstOCC).set_Font(new Font("Courier New", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstOCC).set_FormattingEnabled(true);
			lstOCC.set_HorizontalScrollbar(true);
			lstOCC.set_ItemHeight(12);
			((Control)lstOCC).set_Location(new Point(4, 4));
			((Control)lstOCC).set_Name("lstOCC");
			((Control)lstOCC).set_Size(new Size(991, 292));
			((Control)lstOCC).set_TabIndex(0);
			lstOCC.add_SelectedIndexChanged((EventHandler)lstOCC_SelectedIndexChanged);
			((Control)lstOCC).add_DoubleClick((EventHandler)lstOCC_DoubleClick);
			((Control)cmdToDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdToDoubles).set_Location(new Point(867, 565));
			((Control)cmdToDoubles).set_Name("cmdToDoubles");
			((Control)cmdToDoubles).set_Size(new Size(96, 49));
			((Control)cmdToDoubles).set_TabIndex(8);
			((Control)cmdToDoubles).set_Text("Return to\r\nXZDoubles");
			((ButtonBase)cmdToDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdToDoubles).add_Click((EventHandler)cmdToDoubles_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1017, 669));
			((Control)this).get_Controls().Add((Control)(object)panelDoubles);
			((Control)this).get_Controls().Add((Control)(object)panelOCC);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsXZDoubles", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsXZDoubles);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1033, 635));
			((Control)this).set_Name("XZDoubles_Editor");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Edit XZDoubles.dat");
			((Form)this).add_FormClosing(new FormClosingEventHandler(XZDoubles_Editor_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(XZDoubles_Editor_FormClosed));
			((Form)this).add_Load((EventHandler)XZDoubles_Editor_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)panelDoubles).ResumeLayout(false);
			((Control)panelDoubles).PerformLayout();
			((Control)groupBox8).ResumeLayout(false);
			((Control)panelOCC).ResumeLayout(false);
			((Control)panelOCC).PerformLayout();
			((Control)groupBox7).ResumeLayout(false);
			((Control)groupBox7).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((ISupportInitialize)updnY4).EndInit();
			((ISupportInitialize)updnY3).EndInit();
			((ISupportInitialize)updnY5).EndInit();
			((ISupportInitialize)updnY2).EndInit();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((ISupportInitialize)updnY1).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
