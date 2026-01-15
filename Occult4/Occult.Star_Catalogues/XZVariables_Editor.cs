using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class XZVariables_Editor : Form
	{
		private int CurrentXZ;

		private IContainer components;

		private ListBox lstVars;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private GroupBox groupBox3;

		private Label label2;

		private Label label1;

		private Label label10;

		private Label label9;

		private Label label8;

		private Label label7;

		private Label label6;

		private Label label5;

		private Label label13;

		private Label label12;

		private TextBox txtVarID;

		private TextBox txtXZ;

		private TextBox txtPeriod;

		private TextBox txtEpoch;

		private TextBox txtMinMag;

		private TextBox txtMaxMag;

		private TextBox txtVarType;

		private ComboBox cmbCode;

		private ComboBox cmbPhoto;

		private Button cmdReplace;

		private Button cmdAdd;

		private ToolStripMenuItem helpToolStripMenuItem;

		public XZVariables_Editor()
		{
			InitializeComponent();
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			EditXZ_Forms.ReadVariableStarFile();
			PopulateList(0);
		}

		private void XZVariables_Editor_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((ListControl)cmbCode).set_SelectedIndex(0);
			((ListControl)cmbPhoto).set_SelectedIndex(0);
			EditXZ_Forms.ReadVariableStarFile();
			PopulateList(0);
		}

		internal void PopulateList(int SelectedLine)
		{
			lstVars.BeginUpdate();
			lstVars.get_Items().Clear();
			for (int i = 0; i < EditXZ_Forms.VariableList.Count; i++)
			{
				lstVars.get_Items().Add((object)EditXZ_Forms.VariableList[i].ToString());
			}
			if ((SelectedLine >= 0) & (SelectedLine < lstVars.get_Items().get_Count()))
			{
				((ListControl)lstVars).set_SelectedIndex(SelectedLine);
			}
			lstVars.EndUpdate();
		}

		private void chkNSV_CheckedChanged(object sender, EventArgs e)
		{
		}

		private void lstVars_SelectedIndexChanged(object sender, EventArgs e)
		{
			DecodeLine(((ListControl)lstVars).get_SelectedIndex());
		}

		private void DecodeLine(int Line)
		{
			((Control)txtXZ).set_Text(EditXZ_Forms.VariableList[Line].XZ.ToString());
			((Control)txtVarID).set_Text(EditXZ_Forms.VariableList[Line].Name);
			((Control)txtVarType).set_Text(EditXZ_Forms.VariableList[Line].VType);
			if (EditXZ_Forms.VariableList[Line].MaxMag > -10.0)
			{
				((Control)txtMaxMag).set_Text(Utilities.FormatNumber(EditXZ_Forms.VariableList[Line].MaxMag, 1, EditXZ_Forms.VariableList[Line].MaxMagPlaces));
			}
			else
			{
				((Control)txtMaxMag).set_Text("");
			}
			((ListControl)cmbCode).set_SelectedIndex(0);
			int num = " (<".IndexOf(EditXZ_Forms.VariableList[Line].MinMagFlag.ToString());
			if (num >= 0)
			{
				((ListControl)cmbCode).set_SelectedIndex(num);
			}
			if (EditXZ_Forms.VariableList[Line].MinMag > -10.0)
			{
				((Control)txtMinMag).set_Text(Utilities.FormatNumber(EditXZ_Forms.VariableList[Line].MinMag, 1, EditXZ_Forms.VariableList[Line].MinMagPlaces));
			}
			else
			{
				((Control)txtMinMag).set_Text("");
			}
			((ListControl)cmbPhoto).set_SelectedIndex(0);
			num = "  U B V R I J H K L M RcIcu v b y u'g'r'i'z'pgpvbjrfw C CRCVR1HpT NU".IndexOf(EditXZ_Forms.VariableList[Line].PhotoSystem.ToString().PadRight(2)) / 2;
			if (num >= 0)
			{
				((ListControl)cmbPhoto).set_SelectedIndex(num);
			}
			if (EditXZ_Forms.VariableList[Line].Epoch > 0.0)
			{
				((Control)txtEpoch).set_Text(Utilities.FormatNumber(EditXZ_Forms.VariableList[Line].Epoch, 1, EditXZ_Forms.VariableList[Line].EpochPlaces));
			}
			else
			{
				((Control)txtEpoch).set_Text("");
			}
			if (EditXZ_Forms.VariableList[Line].Period > 0.0)
			{
				((Control)txtPeriod).set_Text(Utilities.FormatNumber(EditXZ_Forms.VariableList[Line].Period, 1, EditXZ_Forms.VariableList[Line].PeriodPlaces));
			}
			else
			{
				((Control)txtPeriod).set_Text("");
			}
		}

		private void EncodeLine(int Line)
		{
			if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
			{
				result = 0;
			}
			if (result > 244437 || result < 0)
			{
				result = 0;
			}
			EditXZ_Forms.VariableList[Line].XZ = (CurrentXZ = result);
			EditXZ_Forms.VariableList[Line].Name = ((Control)txtVarID).get_Text();
			EditXZ_Forms.VariableList[Line].VType = ((Control)txtVarType).get_Text();
			if (!double.TryParse(((Control)txtMaxMag).get_Text(), out var result2))
			{
				result2 = -20.0;
			}
			EditXZ_Forms.VariableList[Line].MaxMag = result2;
			EditXZ_Forms.VariableList[Line].MaxMagPlaces = Utilities.DecimalPlaces(((Control)txtMaxMag).get_Text());
			if (!double.TryParse(((Control)txtMinMag).get_Text(), out result2))
			{
				result2 = -20.0;
			}
			EditXZ_Forms.VariableList[Line].MinMag = result2;
			EditXZ_Forms.VariableList[Line].MinMagPlaces = Utilities.DecimalPlaces(((Control)txtMinMag).get_Text());
			EditXZ_Forms.VariableList[Line].MinMagFlag = cmbCode.get_Items().get_Item(((ListControl)cmbCode).get_SelectedIndex()).ToString();
			EditXZ_Forms.VariableList[Line].PhotoSystem = cmbPhoto.get_Items().get_Item(((ListControl)cmbPhoto).get_SelectedIndex()).ToString();
			if (!double.TryParse(((Control)txtEpoch).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			EditXZ_Forms.VariableList[Line].Epoch = result2;
			EditXZ_Forms.VariableList[Line].EpochPlaces = Utilities.DecimalPlaces(((Control)txtEpoch).get_Text());
			if (!double.TryParse(((Control)txtPeriod).get_Text(), out result2))
			{
				result2 = -20.0;
			}
			EditXZ_Forms.VariableList[Line].Period = result2;
			EditXZ_Forms.VariableList[Line].PeriodPlaces = Utilities.DecimalPlaces(((Control)txtPeriod).get_Text());
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstVars).get_SelectedIndex();
			EncodeLine(selectedIndex);
			PopulateList(selectedIndex);
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			XZVariables item = new XZVariables();
			EditXZ_Forms.VariableList.Add(item);
			EncodeLine(EditXZ_Forms.VariableList.Count - 1);
			EditXZ_Forms.VariableList.Sort();
			int selectedLine = EditXZ_Forms.VariableList.FindIndex(MatchesXZ);
			PopulateList(selectedLine);
		}

		private bool MatchesXZ(XZVariables Variables)
		{
			return Variables.XZ == CurrentXZ;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			EditXZ_Forms.WriteVariableStarFile();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"XZVariables Editor");
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
			//IL_1166: Unknown result type (might be due to invalid IL or missing references)
			//IL_1170: Expected O, but got Unknown
			lstVars = new ListBox();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			groupBox1 = new GroupBox();
			label2 = new Label();
			txtVarID = new TextBox();
			txtXZ = new TextBox();
			label1 = new Label();
			groupBox2 = new GroupBox();
			txtMinMag = new TextBox();
			txtMaxMag = new TextBox();
			txtVarType = new TextBox();
			cmbCode = new ComboBox();
			cmbPhoto = new ComboBox();
			label10 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			label6 = new Label();
			label5 = new Label();
			groupBox3 = new GroupBox();
			txtPeriod = new TextBox();
			txtEpoch = new TextBox();
			label13 = new Label();
			label12 = new Label();
			cmdReplace = new Button();
			cmdAdd = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstVars).set_Anchor((AnchorStyles)1);
			((Control)lstVars).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstVars).set_FormattingEnabled(true);
			lstVars.set_ItemHeight(14);
			((Control)lstVars).set_Location(new Point(10, 30));
			((Control)lstVars).set_Name("lstVars");
			((Control)lstVars).set_Size(new Size(746, 186));
			((Control)lstVars).set_TabIndex(0);
			lstVars.add_SelectedIndexChanged((EventHandler)lstVars_SelectedIndexChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(768, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(58, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...    ");
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)groupBox1).set_Anchor((AnchorStyles)0);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtVarID);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtXZ);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).set_Location(new Point(10, 222));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(228, 115));
			((Control)groupBox1).set_TabIndex(2);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Variable identifier");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(20, 53));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(47, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Identifier");
			((Control)txtVarID).set_Location(new Point(11, 68));
			((Control)txtVarID).set_Name("txtVarID");
			((Control)txtVarID).set_Size(new Size(208, 20));
			((Control)txtVarID).set_TabIndex(7);
			((Control)txtXZ).set_Location(new Point(45, 20));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(59, 20));
			((Control)txtXZ).set_TabIndex(5);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(20, 24));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(21, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("XZ");
			((Control)groupBox2).set_Anchor((AnchorStyles)0);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMinMag);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMaxMag);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtVarType);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbCode);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbPhoto);
			((Control)groupBox2).get_Controls().Add((Control)(object)label10);
			((Control)groupBox2).get_Controls().Add((Control)(object)label9);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)label6);
			((Control)groupBox2).get_Controls().Add((Control)(object)label5);
			((Control)groupBox2).set_Location(new Point(244, 222));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(194, 115));
			((Control)groupBox2).set_TabIndex(3);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Details of the variable - general");
			((Control)txtMinMag).set_Location(new Point(140, 58));
			((Control)txtMinMag).set_Name("txtMinMag");
			((Control)txtMinMag).set_Size(new Size(33, 20));
			((Control)txtMinMag).set_TabIndex(12);
			((Control)txtMaxMag).set_Location(new Point(51, 58));
			((Control)txtMaxMag).set_Name("txtMaxMag");
			((Control)txtMaxMag).set_Size(new Size(33, 20));
			((Control)txtMaxMag).set_TabIndex(11);
			((Control)txtVarType).set_Location(new Point(57, 19));
			((Control)txtVarType).set_Name("txtVarType");
			((Control)txtVarType).set_Size(new Size(117, 20));
			((Control)txtVarType).set_TabIndex(10);
			cmbCode.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbCode).set_FormattingEnabled(true);
			cmbCode.get_Items().AddRange(new object[3] { " ", "(", "<" });
			((Control)cmbCode).set_Location(new Point(97, 58));
			cmbCode.set_MaxDropDownItems(20);
			((Control)cmbCode).set_Name("cmbCode");
			((Control)cmbCode).set_Size(new Size(30, 21));
			((Control)cmbCode).set_TabIndex(9);
			cmbPhoto.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbPhoto).set_FormattingEnabled(true);
			cmbPhoto.get_Items().AddRange(new object[34]
			{
				"  ", "U ", "B ", "V ", "R ", "I ", "J ", "H ", "K ", "L ",
				"M ", "Rc", "Ic", "u ", "v ", "b ", "y ", "u'", "g'", "r'",
				"i'", "z'", "pg", "pv", "bj", "rf", "w ", "C ", "CR", "CV",
				"R1", "Hp", "T ", "NU"
			});
			((Control)cmbPhoto).set_Location(new Point(136, 82));
			cmbPhoto.set_MaxDropDownItems(15);
			((Control)cmbPhoto).set_Name("cmbPhoto");
			((Control)cmbPhoto).set_Size(new Size(37, 21));
			((Control)cmbPhoto).set_TabIndex(8);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(32, 86));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(98, 13));
			((Control)label10).set_TabIndex(6);
			((Control)label10).set_Text("Photometric system");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(20, 62));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(28, 13));
			((Control)label9).set_TabIndex(5);
			((Control)label9).set_Text("Mag");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(4, 21));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(31, 13));
			((Control)label8).set_TabIndex(4);
			((Control)label8).set_Text("Type");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(57, 42));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(27, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Max");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(140, 42));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(24, 13));
			((Control)label6).set_TabIndex(2);
			((Control)label6).set_Text("Min");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(91, 42));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(32, 13));
			((Control)label5).set_TabIndex(1);
			((Control)label5).set_Text("Code");
			((Control)groupBox3).set_Anchor((AnchorStyles)0);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtPeriod);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtEpoch);
			((Control)groupBox3).get_Controls().Add((Control)(object)label13);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).set_Location(new Point(444, 222));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(202, 115));
			((Control)groupBox3).set_TabIndex(3);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Details of periodicity");
			((Control)txtPeriod).set_Location(new Point(80, 46));
			((Control)txtPeriod).set_Name("txtPeriod");
			((Control)txtPeriod).set_Size(new Size(111, 20));
			((Control)txtPeriod).set_TabIndex(5);
			((Control)txtEpoch).set_Location(new Point(78, 22));
			((Control)txtEpoch).set_Name("txtEpoch");
			((Control)txtEpoch).set_Size(new Size(114, 20));
			((Control)txtEpoch).set_TabIndex(4);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(6, 22));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(60, 13));
			((Control)label13).set_TabIndex(3);
			((Control)label13).set_Text("Epoch (JD)");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(6, 46));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(68, 13));
			((Control)label12).set_TabIndex(2);
			((Control)label12).set_Text("Period (days)");
			((Control)cmdReplace).set_Anchor((AnchorStyles)0);
			((Control)cmdReplace).set_Location(new Point(663, 233));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(81, 47));
			((Control)cmdReplace).set_TabIndex(4);
			((Control)cmdReplace).set_Text("Replace\r\nentry");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdAdd).set_Anchor((AnchorStyles)0);
			((Control)cmdAdd).set_Location(new Point(663, 288));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(81, 47));
			((Control)cmdAdd).set_TabIndex(5);
			((Control)cmdAdd).set_Text("Add as\r\nnew entry");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(768, 364));
			((Control)this).get_Controls().Add((Control)(object)cmdAdd);
			((Control)this).get_Controls().Add((Control)(object)cmdReplace);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)lstVars);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsXZVariables", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsXZVariables);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(690, 400));
			((Control)this).set_Name("XZVariables_Editor");
			((Control)this).set_Text("XZVariables Editor");
			((Form)this).add_Load((EventHandler)XZVariables_Editor_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
