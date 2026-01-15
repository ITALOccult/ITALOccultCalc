using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult
{
	public class AsteroidRings : Form
	{
		private readonly string AppPath;

		private readonly string RingFileHeader;

		public int FirstRecord;

		public int LastRecord;

		private const int ElementFileLength = 67;

		internal static int CurrentlySelectedIndex;

		private bool Edited;

		private IContainer components;

		private ListBox lstRingElements;

		private Label label1;

		private MenuStrip menuStrip1;

		private Label label4;

		private TextBox txtAsteroidNumber;

		private Label label2;

		private TextBox txtDec;

		private Label label3;

		private TextBox txtRA;

		private Label label5;

		private Label label6;

		private TextBox txtRadius1;

		private TextBox txtRadius5;

		private TextBox txtRadius4;

		private TextBox txtRadius3;

		private TextBox txtRadius2;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label lblNumRings;

		private Button cmdAdd;

		private Button cmdReplace;

		private Button cmdDelete;

		private Button cmdSave;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem reloadToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdExit;

		public AsteroidRings()
		{
			InitializeComponent();
			RingFileHeader = "Asteroid number,Pole RA° deg,Pole Dec°,#rings,Radius 1,Radius 2,Radius 3,Radius 4,Radius 5";
			LoadRingFile();
		}

		private void AsteroidRings_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			if (!File.Exists(AsteroidRings_All.SourceFile))
			{
				Downloads.DownloadAsteroidRings();
			}
			if (File.Exists(AsteroidRings_All.SourceFile))
			{
				((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
				LoadRingFile();
			}
		}

		private void LoadRingFile()
		{
			AsteroidRings_All.Fill_AllAsteroidRings();
			FillListElements();
			if (lstRingElements.get_Items().get_Count() > 0)
			{
				((ListControl)lstRingElements).set_SelectedIndex(0);
				SetEditBoxes();
			}
		}

		private void FillListElements()
		{
			lstRingElements.get_Items().Clear();
			for (int i = 0; i < AsteroidRings_All.AsteroidRings.Count; i++)
			{
				lstRingElements.get_Items().Add((object)AsteroidRings_All.AsteroidRings[i].RingLineForDisplay());
			}
		}

		private void lstRingElements_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetEditBoxes();
		}

		private void SetEditBoxes()
		{
			CurrentlySelectedIndex = ((ListControl)lstRingElements).get_SelectedIndex();
			if (CurrentlySelectedIndex >= 0)
			{
				((Control)txtAsteroidNumber).set_Text(AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].IDAsteroidNumber.ToString());
				((Control)txtRA).set_Text(string.Format("{0,1:f2}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].RA_Pole));
				((Control)txtDec).set_Text(string.Format("{0,1:f2}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Dec_Pole));
				((Control)txtRadius1).set_Text(string.Format("{0,1:f1}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Radius1));
				((Control)txtRadius2).set_Text(string.Format("{0,1:f1}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Radius2));
				((Control)txtRadius3).set_Text(string.Format("{0,1:f1}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Radius3));
				((Control)txtRadius4).set_Text(string.Format("{0,1:f1}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Radius4));
				((Control)txtRadius5).set_Text(string.Format("{0,1:f1}", AsteroidRings_All.AsteroidRings[CurrentlySelectedIndex].Radius5));
				((Control)lblNumRings).set_Text(Ring_Count() + " rings");
			}
		}

		private int Ring_Count()
		{
			int num = 0;
			double result = 0.0;
			if (!double.TryParse(((Control)txtRadius1).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result > 0.0)
			{
				num++;
			}
			if (!double.TryParse(((Control)txtRadius2).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result > 0.0)
			{
				num++;
			}
			if (!double.TryParse(((Control)txtRadius3).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result > 0.0)
			{
				num++;
			}
			if (!double.TryParse(((Control)txtRadius4).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result > 0.0)
			{
				num++;
			}
			if (!double.TryParse(((Control)txtRadius5).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result > 0.0)
			{
				num++;
			}
			return num;
		}

		private void CreateLine(bool Replace)
		{
			AsteroidRingDetails asteroidRingDetails = new AsteroidRingDetails();
			if (!int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result))
			{
				result = 0;
			}
			asteroidRingDetails.IDAsteroidNumber = result;
			if (!double.TryParse(((Control)txtRA).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			asteroidRingDetails.RA_Pole = result2;
			if (!double.TryParse(((Control)txtDec).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			asteroidRingDetails.Dec_Pole = result3;
			asteroidRingDetails.NumberOfRings = Ring_Count();
			if (!double.TryParse(((Control)txtRadius1).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			asteroidRingDetails.Radius1 = result4;
			if (!double.TryParse(((Control)txtRadius2).get_Text(), out var result5))
			{
				result5 = 0.0;
			}
			asteroidRingDetails.Radius2 = result5;
			if (!double.TryParse(((Control)txtRadius3).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			asteroidRingDetails.Radius3 = result6;
			if (!double.TryParse(((Control)txtRadius4).get_Text(), out var result7))
			{
				result7 = 0.0;
			}
			asteroidRingDetails.Radius4 = result7;
			if (!double.TryParse(((Control)txtRadius5).get_Text(), out var result8))
			{
				result8 = 0.0;
			}
			asteroidRingDetails.Radius5 = result8;
			if (Replace)
			{
				AsteroidRings_All.AsteroidRings.RemoveAt(CurrentlySelectedIndex);
			}
			AsteroidRings_All.AsteroidRings.Add(asteroidRingDetails);
			AsteroidRings_All.AsteroidRings.Sort();
			FillListElements();
			((ListControl)lstRingElements).set_SelectedIndex(CurrentlySelectedIndex);
			Application.DoEvents();
			SetEditBoxes();
			Edited = true;
		}

		private void txtRadius1_Leave(object sender, EventArgs e)
		{
			((Control)lblNumRings).set_Text(Ring_Count() + " rings");
		}

		private void txtRadius2_Leave(object sender, EventArgs e)
		{
			((Control)lblNumRings).set_Text(Ring_Count() + " rings");
		}

		private void txtRadius3_Leave(object sender, EventArgs e)
		{
			((Control)lblNumRings).set_Text(Ring_Count() + " rings");
		}

		private void txtRadius4_Leave(object sender, EventArgs e)
		{
			((Control)lblNumRings).set_Text(Ring_Count() + " rings");
		}

		private void txtRadius5_Leave(object sender, EventArgs e)
		{
			((Control)lblNumRings).set_Text(Ring_Count() + " rings");
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			CreateLine(Replace: false);
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to REPLACE", "Check Replace", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				CreateLine(Replace: true);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_004e: Invalid comparison between Unknown and I4
			((ListControl)lstRingElements).get_SelectedIndex();
			if ((int)MessageBox.Show("Are you sure you want to Delete \r\n\r\n" + lstRingElements.get_Items().get_Item(((ListControl)lstRingElements).get_SelectedIndex()).ToString(), "Check Delete", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				AsteroidRings_All.AsteroidRings.RemoveAt(CurrentlySelectedIndex);
				AsteroidRings_All.AsteroidRings.Sort();
				FillListElements();
				Edited = true;
				if (CurrentlySelectedIndex > 0)
				{
					CurrentlySelectedIndex--;
				}
				((ListControl)lstRingElements).set_SelectedIndex(CurrentlySelectedIndex);
				Application.DoEvents();
				SetEditBoxes();
			}
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void Save()
		{
			if (lstRingElements.get_Items().get_Count() >= 1)
			{
				string text = AsteroidRings_All.SourceFile.Replace(".csv", ".bup");
				if (File.Exists(text))
				{
					File.Delete(text);
				}
				File.Move(AsteroidRings_All.SourceFile, text);
				StreamWriter streamWriter = new StreamWriter(AsteroidRings_All.SourceFile);
				lstRingElements.set_Sorted(true);
				streamWriter.WriteLine(RingFileHeader);
				for (int i = 0; i < lstRingElements.get_Items().get_Count(); i++)
				{
					streamWriter.WriteLine(AsteroidRings_All.AsteroidRings[i].ToString());
				}
				streamWriter.Close();
				Edited = false;
			}
		}

		private void AsteroidRings_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_0028: Invalid comparison between Unknown and I4
			if (Edited)
			{
				DialogResult val = MessageBox.Show("Some changes to the data has not been Saved\n\nDo you want to Save the data?", "Changes have not been saved!", (MessageBoxButtons)3, (MessageBoxIcon)48);
				if ((int)val == 6)
				{
					Save();
				}
				else if ((int)val == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
				}
			}
		}

		private void reloadToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to re-load, and loose all edits?", "Re-Load", (MessageBoxButtons)1, (MessageBoxIcon)32) != 2)
			{
				LoadRingFile();
			}
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid Rings");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
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
			//IL_117b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1185: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidRings));
			lstRingElements = new ListBox();
			label1 = new Label();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			reloadToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label4 = new Label();
			txtAsteroidNumber = new TextBox();
			label2 = new Label();
			txtDec = new TextBox();
			label3 = new Label();
			txtRA = new TextBox();
			label5 = new Label();
			label6 = new Label();
			txtRadius1 = new TextBox();
			txtRadius5 = new TextBox();
			txtRadius4 = new TextBox();
			txtRadius3 = new TextBox();
			txtRadius2 = new TextBox();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			lblNumRings = new Label();
			cmdAdd = new Button();
			cmdReplace = new Button();
			cmdDelete = new Button();
			cmdSave = new Button();
			cmdExit = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstRingElements).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstRingElements).set_FormattingEnabled(true);
			lstRingElements.set_ItemHeight(14);
			((Control)lstRingElements).set_Location(new Point(12, 64));
			((Control)lstRingElements).set_Name("lstRingElements");
			((Control)lstRingElements).set_Size(new Size(481, 172));
			lstRingElements.set_Sorted(true);
			((Control)lstRingElements).set_TabIndex(3);
			lstRingElements.add_SelectedIndexChanged((EventHandler)lstRingElements_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(15, 44));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(455, 14));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("     No    RA     Dec   #      1       2       3       4       5");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(505, 24));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)reloadToolStripMenuItem });
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(55, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...   ");
			((ToolStripItem)reloadToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)reloadToolStripMenuItem).set_Name("reloadToolStripMenuItem");
			((ToolStripItem)reloadToolStripMenuItem).set_Size(new Size(112, 22));
			((ToolStripItem)reloadToolStripMenuItem).set_Text("re-load");
			((ToolStripItem)reloadToolStripMenuItem).add_Click((EventHandler)reloadToolStripMenuItem_Click);
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
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(17, 247));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(83, 13));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("Asteroid number");
			((Control)txtAsteroidNumber).set_Location(new Point(102, 244));
			((TextBoxBase)txtAsteroidNumber).set_MaxLength(7);
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(63, 20));
			((Control)txtAsteroidNumber).set_TabIndex(7);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(42, 316));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(54, 13));
			((Control)label2).set_TabIndex(8);
			((Control)label2).set_Text("Dec (deg)");
			((Control)txtDec).set_Location(new Point(102, 313));
			((TextBoxBase)txtDec).set_MaxLength(7);
			((Control)txtDec).set_Name("txtDec");
			((Control)txtDec).set_Size(new Size(63, 20));
			((Control)txtDec).set_TabIndex(9);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(47, 292));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(49, 13));
			((Control)label3).set_TabIndex(10);
			((Control)label3).set_Text("RA (deg)");
			((Control)txtRA).set_Location(new Point(102, 289));
			((TextBoxBase)txtRA).set_MaxLength(7);
			((Control)txtRA).set_Name("txtRA");
			((Control)txtRA).set_Size(new Size(63, 20));
			((Control)txtRA).set_TabIndex(11);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(59, 273));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(84, 13));
			((Control)label5).set_TabIndex(12);
			((Control)label5).set_Text("Location of Pole");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(15, 30));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(315, 14));
			((Control)label6).set_TabIndex(13);
			((Control)label6).set_Text("  Aster    Pole-(deg) rings      radius (km)");
			((Control)txtRadius1).set_Location(new Point(190, 277));
			((TextBoxBase)txtRadius1).set_MaxLength(7);
			((Control)txtRadius1).set_Name("txtRadius1");
			((Control)txtRadius1).set_Size(new Size(55, 20));
			((Control)txtRadius1).set_TabIndex(14);
			((Control)txtRadius1).add_Leave((EventHandler)txtRadius1_Leave);
			((Control)txtRadius5).set_Location(new Point(434, 277));
			((TextBoxBase)txtRadius5).set_MaxLength(7);
			((Control)txtRadius5).set_Name("txtRadius5");
			((Control)txtRadius5).set_Size(new Size(55, 20));
			((Control)txtRadius5).set_TabIndex(15);
			((Control)txtRadius5).add_Leave((EventHandler)txtRadius5_Leave);
			((Control)txtRadius4).set_Location(new Point(373, 277));
			((TextBoxBase)txtRadius4).set_MaxLength(7);
			((Control)txtRadius4).set_Name("txtRadius4");
			((Control)txtRadius4).set_Size(new Size(55, 20));
			((Control)txtRadius4).set_TabIndex(16);
			((Control)txtRadius4).add_Leave((EventHandler)txtRadius4_Leave);
			((Control)txtRadius3).set_Location(new Point(312, 277));
			((TextBoxBase)txtRadius3).set_MaxLength(7);
			((Control)txtRadius3).set_Name("txtRadius3");
			((Control)txtRadius3).set_Size(new Size(55, 20));
			((Control)txtRadius3).set_TabIndex(17);
			((Control)txtRadius3).add_Leave((EventHandler)txtRadius3_Leave);
			((Control)txtRadius2).set_Location(new Point(251, 277));
			((TextBoxBase)txtRadius2).set_MaxLength(7);
			((Control)txtRadius2).set_Name("txtRadius2");
			((Control)txtRadius2).set_Size(new Size(55, 20));
			((Control)txtRadius2).set_TabIndex(18);
			((Control)txtRadius2).add_Leave((EventHandler)txtRadius2_Leave);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(276, 244));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(118, 13));
			((Control)label7).set_TabIndex(19);
			((Control)label7).set_Text("Radius of the rings (km)");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(211, 262));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(13, 13));
			((Control)label8).set_TabIndex(20);
			((Control)label8).set_Text("1");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(272, 262));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(13, 13));
			((Control)label9).set_TabIndex(21);
			((Control)label9).set_Text("2");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(333, 262));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(13, 13));
			((Control)label10).set_TabIndex(22);
			((Control)label10).set_Text("3");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(394, 262));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(13, 13));
			((Control)label11).set_TabIndex(23);
			((Control)label11).set_Text("4");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(455, 261));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(24);
			((Control)label12).set_Text("5");
			((Control)lblNumRings).set_AutoSize(true);
			lblNumRings.set_BorderStyle((BorderStyle)1);
			((Control)lblNumRings).set_Location(new Point(434, 243));
			((Control)lblNumRings).set_Name("lblNumRings");
			((Control)lblNumRings).set_Size(new Size(43, 15));
			((Control)lblNumRings).set_TabIndex(25);
			((Control)lblNumRings).set_Text("label13");
			((Control)cmdAdd).set_Location(new Point(190, 309));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(55, 20));
			((Control)cmdAdd).set_TabIndex(26);
			((Control)cmdAdd).set_Text("Add");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdReplace).set_Location(new Point(251, 309));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(55, 20));
			((Control)cmdReplace).set_TabIndex(27);
			((Control)cmdReplace).set_Text("Replace");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdDelete).set_Location(new Point(312, 309));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(55, 20));
			((Control)cmdDelete).set_TabIndex(28);
			((Control)cmdDelete).set_Text("Delete");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)cmdSave).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSave).set_Location(new Point(373, 309));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(55, 20));
			((Control)cmdSave).set_TabIndex(29);
			((Control)cmdSave).set_Text("Save");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)cmdExit).set_Location(new Point(434, 309));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(55, 20));
			((Control)cmdExit).set_TabIndex(30);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(505, 341));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdSave);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Control)this).get_Controls().Add((Control)(object)cmdReplace);
			((Control)this).get_Controls().Add((Control)(object)cmdAdd);
			((Control)this).get_Controls().Add((Control)(object)lblNumRings);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)txtRadius2);
			((Control)this).get_Controls().Add((Control)(object)txtRadius3);
			((Control)this).get_Controls().Add((Control)(object)txtRadius4);
			((Control)this).get_Controls().Add((Control)(object)txtRadius5);
			((Control)this).get_Controls().Add((Control)(object)txtRadius1);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)txtRA);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtDec);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstRingElements);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidRings");
			((Control)this).set_Text("Asteroid Rings");
			((Form)this).add_FormClosing(new FormClosingEventHandler(AsteroidRings_FormClosing));
			((Form)this).add_Load((EventHandler)AsteroidRings_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
