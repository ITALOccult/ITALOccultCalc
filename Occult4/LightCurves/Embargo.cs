using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult;
using Occult.Properties;

namespace LightCurves
{
	public class Embargo : Form
	{
		private int CurrentlySelectedRecord = -1;

		private IContainer components;

		private ListBox lstEmbargo;

		private ListBox lstLightCurves;

		private Button cmdDelete;

		private GroupBox grpAdd;

		private TextBox txtEndYear;

		private TextBox txtEndMonth;

		private TextBox txtStartMonth;

		private TextBox txtStartYear;

		private Label label3;

		private Label label2;

		private Label label1;

		private TextBox txtAsteroid;

		private Button cmdAdd;

		private Label label6;

		private Label label5;

		private TextBox txtContact;

		private Label label4;

		private TextBox txtRequestor;

		private Label label7;

		private Label label8;

		private Button cmdReplace;

		private Button cmdHelp;

		public Embargo()
		{
			InitializeComponent();
		}

		private void Embargo_Load(object sender, EventArgs e)
		{
			SetNewEntry();
			ReadEmbargoes();
		}

		private void SetNewEntry()
		{
			((Control)txtAsteroid).set_Text("1");
			((Control)txtStartYear).set_Text(DateTime.UtcNow.AddMonths(-6).Year.ToString());
			((Control)txtStartMonth).set_Text(DateTime.UtcNow.AddMonths(-6).Month.ToString());
			((Control)txtEndYear).set_Text(DateTime.UtcNow.AddMonths(18).Year.ToString());
			((Control)txtEndMonth).set_Text(DateTime.UtcNow.AddMonths(18).Month.ToString());
			((Control)txtRequestor).set_Text("");
			((Control)txtContact).set_Text("");
		}

		internal void ReadEmbargoes()
		{
			lstEmbargo.get_Items().Clear();
			lstLightCurves.get_Items().Clear();
			EmbargoOps.ReadEmbargoes();
			FillListOfEmbargoes(Save: false);
		}

		private void FillListOfEmbargoes(bool Save)
		{
			if (Save)
			{
				SaveListOfEmbargoes();
			}
			lstEmbargo.get_Items().Clear();
			lstEmbargo.get_Items().Add((object)Embargoed.Header_View);
			if (EmbargoOps.EmbargoedList.Count < 1)
			{
				lstEmbargo.get_Items().Add((object)"No embargoes");
			}
			else
			{
				for (int i = 0; i < EmbargoOps.EmbargoedList.Count; i++)
				{
					lstEmbargo.get_Items().Add((object)EmbargoOps.EmbargoedList[i].DisplayLine);
				}
			}
			Application.DoEvents();
			EmbargoOps.SetClearEmbargoStatus();
			FillLightCurveList();
		}

		private void SaveListOfEmbargoes()
		{
			EmbargoOps.SaveListOfEmbargoes();
		}

		private void FillLightCurveList()
		{
			lstLightCurves.get_Items().Clear();
			string[] files = Directory.GetFiles(EmbargoOps.LightCurveReportsDirectory, "*.embargoed");
			foreach (string path in files)
			{
				lstLightCurves.get_Items().Add((object)Path.GetFileName(path));
			}
		}

		private void txtAsteroid_KeyDown(object sender, KeyEventArgs e)
		{
		}

		private void txtStartYear_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtStartMonth_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtEndYear_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtEndMonth_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void lstEmbargo_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstEmbargo).get_SelectedIndex() > 0)
			{
				FillFields(((ListControl)lstEmbargo).get_SelectedIndex() - 1);
				CurrentlySelectedRecord = ((ListControl)lstEmbargo).get_SelectedIndex() - 1;
			}
			else
			{
				SetNewEntry();
			}
		}

		private void FillFields(int Rec)
		{
			((Control)txtAsteroid).set_Text(EmbargoOps.EmbargoedList[Rec].AsteroidNumber);
			((Control)txtStartYear).set_Text(EmbargoOps.EmbargoedList[Rec].YearStart.ToString());
			((Control)txtStartMonth).set_Text(EmbargoOps.EmbargoedList[Rec].MonthStart.ToString());
			((Control)txtEndYear).set_Text(EmbargoOps.EmbargoedList[Rec].YearEnd.ToString());
			((Control)txtEndMonth).set_Text(EmbargoOps.EmbargoedList[Rec].MonthEnd.ToString());
			((Control)txtRequestor).set_Text(EmbargoOps.EmbargoedList[Rec].Requestor);
			((Control)txtContact).set_Text(EmbargoOps.EmbargoedList[Rec].ContactInfo);
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			Embargoed embargoed = new Embargoed();
			embargoed.AsteroidNumber = ((Control)txtAsteroid).get_Text();
			int.TryParse(((Control)txtStartYear).get_Text(), out var result);
			embargoed.YearStart = result;
			int.TryParse(((Control)txtStartMonth).get_Text(), out result);
			embargoed.MonthStart = result;
			int.TryParse(((Control)txtEndYear).get_Text(), out result);
			embargoed.YearEnd = result;
			int.TryParse(((Control)txtEndMonth).get_Text(), out result);
			embargoed.MonthEnd = result;
			embargoed.Requestor = ((Control)txtRequestor).get_Text().Replace(",", "_");
			embargoed.ContactInfo = ((Control)txtContact).get_Text().Replace(",", "_");
			EmbargoOps.EmbargoedList.Add(embargoed);
			EmbargoOps.EmbargoedList.Sort();
			FillListOfEmbargoes(Save: true);
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].AsteroidNumber = ((Control)txtAsteroid).get_Text();
			int.TryParse(((Control)txtStartYear).get_Text(), out var result);
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].YearStart = result;
			int.TryParse(((Control)txtStartMonth).get_Text(), out result);
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].MonthStart = result;
			int.TryParse(((Control)txtEndYear).get_Text(), out result);
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].YearEnd = result;
			int.TryParse(((Control)txtEndMonth).get_Text(), out result);
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].MonthEnd = result;
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].Requestor = ((Control)txtRequestor).get_Text();
			EmbargoOps.EmbargoedList[CurrentlySelectedRecord].ContactInfo = ((Control)txtContact).get_Text();
			EmbargoOps.EmbargoedList.Sort();
			FillListOfEmbargoes(Save: true);
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			EmbargoOps.EmbargoedList.RemoveAt(CurrentlySelectedRecord);
			EmbargoOps.EmbargoedList.Sort();
			FillListOfEmbargoes(Save: true);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Light Curve Embargo");
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
			//IL_07ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b7: Expected O, but got Unknown
			//IL_080b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0815: Expected O, but got Unknown
			//IL_0869: Unknown result type (might be due to invalid IL or missing references)
			//IL_0873: Expected O, but got Unknown
			//IL_08c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ce: Expected O, but got Unknown
			//IL_0a3e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a48: Expected O, but got Unknown
			lstEmbargo = new ListBox();
			lstLightCurves = new ListBox();
			cmdDelete = new Button();
			grpAdd = new GroupBox();
			cmdReplace = new Button();
			cmdAdd = new Button();
			label6 = new Label();
			label5 = new Label();
			txtContact = new TextBox();
			label4 = new Label();
			txtRequestor = new TextBox();
			txtEndYear = new TextBox();
			txtEndMonth = new TextBox();
			txtStartMonth = new TextBox();
			txtStartYear = new TextBox();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtAsteroid = new TextBox();
			label7 = new Label();
			label8 = new Label();
			cmdHelp = new Button();
			((Control)grpAdd).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEmbargo).set_Font(new Font("Cascadia Code", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEmbargo).set_FormattingEnabled(true);
			lstEmbargo.set_ItemHeight(17);
			((Control)lstEmbargo).set_Location(new Point(16, 179));
			((Control)lstEmbargo).set_Name("lstEmbargo");
			lstEmbargo.set_ScrollAlwaysVisible(true);
			((Control)lstEmbargo).set_Size(new Size(590, 89));
			((Control)lstEmbargo).set_TabIndex(0);
			lstEmbargo.add_SelectedIndexChanged((EventHandler)lstEmbargo_SelectedIndexChanged);
			((Control)lstLightCurves).set_Font(new Font("Cascadia Code", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstLightCurves).set_FormattingEnabled(true);
			lstLightCurves.set_ItemHeight(16);
			((Control)lstLightCurves).set_Location(new Point(16, 312));
			((Control)lstLightCurves).set_Name("lstLightCurves");
			lstLightCurves.set_ScrollAlwaysVisible(true);
			((Control)lstLightCurves).set_Size(new Size(329, 116));
			((Control)lstLightCurves).set_TabIndex(1);
			((Control)cmdDelete).set_BackColor(Color.PeachPuff);
			((Control)cmdDelete).set_Location(new Point(446, 85));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(149, 30));
			((Control)cmdDelete).set_TabIndex(2);
			((Control)cmdDelete).set_Text("Delete selected embargo");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(false);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)grpAdd).get_Controls().Add((Control)(object)cmdReplace);
			((Control)grpAdd).get_Controls().Add((Control)(object)cmdAdd);
			((Control)grpAdd).get_Controls().Add((Control)(object)cmdDelete);
			((Control)grpAdd).get_Controls().Add((Control)(object)label6);
			((Control)grpAdd).get_Controls().Add((Control)(object)label5);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtContact);
			((Control)grpAdd).get_Controls().Add((Control)(object)label4);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtRequestor);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtEndYear);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtEndMonth);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtStartMonth);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtStartYear);
			((Control)grpAdd).get_Controls().Add((Control)(object)label3);
			((Control)grpAdd).get_Controls().Add((Control)(object)label2);
			((Control)grpAdd).get_Controls().Add((Control)(object)label1);
			((Control)grpAdd).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)grpAdd).set_Location(new Point(16, 22));
			((Control)grpAdd).set_Name("grpAdd");
			((Control)grpAdd).set_Size(new Size(600, 125));
			((Control)grpAdd).set_TabIndex(3);
			grpAdd.set_TabStop(false);
			((Control)grpAdd).set_Text("Add/Edit an embargo");
			((Control)cmdReplace).set_BackColor(Color.PaleGreen);
			((Control)cmdReplace).set_Location(new Point(446, 51));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(149, 30));
			((Control)cmdReplace).set_TabIndex(14);
			((Control)cmdReplace).set_Text("Replace selected embargo");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(false);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdAdd).set_BackColor(Color.PaleGreen);
			((Control)cmdAdd).set_Location(new Point(446, 17));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(149, 30));
			((Control)cmdAdd).set_TabIndex(13);
			((Control)cmdAdd).set_Text("Add as New embargo");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(false);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(227, 70));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(35, 13));
			((Control)label6).set_TabIndex(12);
			((Control)label6).set_Text("Name");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(218, 94));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(44, 13));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("Contact");
			((Control)txtContact).set_Location(new Point(270, 90));
			((Control)txtContact).set_Name("txtContact");
			((Control)txtContact).set_Size(new Size(159, 20));
			((Control)txtContact).set_TabIndex(10);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(270, 45));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(56, 13));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("Requestor");
			((Control)txtRequestor).set_Location(new Point(270, 66));
			((Control)txtRequestor).set_Name("txtRequestor");
			((Control)txtRequestor).set_Size(new Size(111, 20));
			((Control)txtRequestor).set_TabIndex(8);
			((Control)txtEndYear).set_Location(new Point(121, 90));
			((Control)txtEndYear).set_Name("txtEndYear");
			((Control)txtEndYear).set_Size(new Size(45, 20));
			((Control)txtEndYear).set_TabIndex(7);
			((Control)txtEndYear).add_KeyPress(new KeyPressEventHandler(txtEndYear_KeyPress));
			((Control)txtEndMonth).set_Location(new Point(171, 90));
			((Control)txtEndMonth).set_Name("txtEndMonth");
			((Control)txtEndMonth).set_Size(new Size(30, 20));
			((Control)txtEndMonth).set_TabIndex(6);
			((Control)txtEndMonth).add_KeyPress(new KeyPressEventHandler(txtEndMonth_KeyPress));
			((Control)txtStartMonth).set_Location(new Point(171, 66));
			((Control)txtStartMonth).set_Name("txtStartMonth");
			((Control)txtStartMonth).set_Size(new Size(30, 20));
			((Control)txtStartMonth).set_TabIndex(5);
			((Control)txtStartMonth).add_KeyPress(new KeyPressEventHandler(txtStartMonth_KeyPress));
			((Control)txtStartYear).set_Location(new Point(121, 66));
			((Control)txtStartYear).set_Name("txtStartYear");
			((Control)txtStartYear).set_Size(new Size(45, 20));
			((Control)txtStartYear).set_TabIndex(4);
			((Control)txtStartYear).add_KeyPress(new KeyPressEventHandler(txtStartYear_KeyPress));
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(10, 94));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(105, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("End Year and Month");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(7, 70));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(108, 13));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text("Start Year and Month");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(32, 34));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(83, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Asteroid number");
			((Control)txtAsteroid).set_Location(new Point(121, 30));
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((Control)txtAsteroid).set_Size(new Size(59, 20));
			((Control)txtAsteroid).set_TabIndex(0);
			((Control)txtAsteroid).add_KeyDown(new KeyEventHandler(txtAsteroid_KeyDown));
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(19, 160));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(147, 17));
			((Control)label7).set_TabIndex(4);
			((Control)label7).set_Text("Current embargoes");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(19, 293));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(125, 17));
			((Control)label8).set_TabIndex(5);
			((Control)label8).set_Text("Embargoed files");
			((Control)cmdHelp).set_BackColor(Color.FromArgb(192, 255, 255));
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(555, 2));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(56, 23));
			((Control)cmdHelp).set_TabIndex(6);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(631, 436));
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)grpAdd);
			((Control)this).get_Controls().Add((Control)(object)lstLightCurves);
			((Control)this).get_Controls().Add((Control)(object)lstEmbargo);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Embargo");
			((Control)this).set_Text("Embargoed light curves");
			((Form)this).add_Load((EventHandler)Embargo_Load);
			((Control)grpAdd).ResumeLayout(false);
			((Control)grpAdd).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
