using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class PDS_AuthorList : Form
	{
		private IContainer components;

		private ListBox List;

		private Button cmdUp;

		private Button cmdDown;

		private Button cmdDelete;

		private TextBox txtNewName;

		private Button cmdAdd;

		private Label label1;

		private Button cmdSave;

		private Button cmdCancel;

		public PDS_AuthorList()
		{
			InitializeComponent();
		}

		private void PDS_AuthorList_Load(object sender, EventArgs e)
		{
			string[] array = Settings.Default.PDSauthors.Replace('"', ' ').Trim().Split(new char[1] { ';' });
			for (int i = 0; i < array.Length; i++)
			{
				List.get_Items().Add((object)array[i].Trim());
			}
		}

		private void cmdUp_Click(object sender, EventArgs e)
		{
			if (((ListControl)List).get_SelectedIndex() > 0)
			{
				string text = List.get_Items().get_Item(((ListControl)List).get_SelectedIndex() - 1).ToString();
				List.get_Items().set_Item(((ListControl)List).get_SelectedIndex() - 1, List.get_Items().get_Item(((ListControl)List).get_SelectedIndex()));
				List.get_Items().set_Item(((ListControl)List).get_SelectedIndex(), (object)text);
				ListBox list = List;
				((ListControl)list).set_SelectedIndex(((ListControl)list).get_SelectedIndex() - 1);
			}
		}

		private void cmdDown_Click(object sender, EventArgs e)
		{
			if (((ListControl)List).get_SelectedIndex() < List.get_Items().get_Count() - 1)
			{
				string text = List.get_Items().get_Item(((ListControl)List).get_SelectedIndex() + 1).ToString();
				List.get_Items().set_Item(((ListControl)List).get_SelectedIndex() + 1, List.get_Items().get_Item(((ListControl)List).get_SelectedIndex()));
				List.get_Items().set_Item(((ListControl)List).get_SelectedIndex(), (object)text);
				ListBox list = List;
				((ListControl)list).set_SelectedIndex(((ListControl)list).get_SelectedIndex() + 1);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to delete\r\n\r\n" + List.get_Items().get_Item(((ListControl)List).get_SelectedIndex()).ToString(), "Confirm delete", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				List.get_Items().RemoveAt(((ListControl)List).get_SelectedIndex());
				((ListControl)List).set_SelectedIndex(0);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			string x = ((Control)txtNewName).get_Text().Trim();
			x = Utilities.ProperCase(x);
			((Control)txtNewName).set_Text(x);
			int length = x.Length;
			int num = x.IndexOf(",");
			if (x.IndexOf(".") != length - 1 || num != length - 4)
			{
				MessageBox.Show("The locations of the comma and period are not as expected. Please review the name", "Format issue", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				List.get_Items().Add((object)x);
			}
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			string text = "\"";
			for (int i = 0; i < List.get_Items().get_Count(); i++)
			{
				if (i > 0)
				{
					text += ";";
				}
				text += List.get_Items().get_Item(i).ToString();
			}
			text += "\"";
			Settings.Default.PDSauthors = text;
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PDS_AuthorList));
			List = new ListBox();
			cmdUp = new Button();
			cmdDown = new Button();
			cmdDelete = new Button();
			txtNewName = new TextBox();
			cmdAdd = new Button();
			label1 = new Label();
			cmdSave = new Button();
			cmdCancel = new Button();
			((Control)this).SuspendLayout();
			((Control)List).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)List).set_FormattingEnabled(true);
			((Control)List).set_Location(new Point(12, 138));
			((Control)List).set_Name("List");
			((Control)List).set_Size(new Size(174, 290));
			((Control)List).set_TabIndex(0);
			((Control)cmdUp).set_Location(new Point(202, 139));
			((Control)cmdUp).set_Name("cmdUp");
			((Control)cmdUp).set_Size(new Size(93, 26));
			((Control)cmdUp).set_TabIndex(1);
			((Control)cmdUp).set_Text("Move UP");
			((ButtonBase)cmdUp).set_UseVisualStyleBackColor(true);
			((Control)cmdUp).add_Click((EventHandler)cmdUp_Click);
			((Control)cmdDown).set_Location(new Point(202, 171));
			((Control)cmdDown).set_Name("cmdDown");
			((Control)cmdDown).set_Size(new Size(93, 26));
			((Control)cmdDown).set_TabIndex(2);
			((Control)cmdDown).set_Text("Move DOWN");
			((ButtonBase)cmdDown).set_UseVisualStyleBackColor(true);
			((Control)cmdDown).add_Click((EventHandler)cmdDown_Click);
			((Control)cmdDelete).set_Location(new Point(202, 227));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(129, 26));
			((Control)cmdDelete).set_TabIndex(3);
			((Control)cmdDelete).set_Text("Delete selected name");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)txtNewName).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtNewName).set_Location(new Point(202, 307));
			((Control)txtNewName).set_Name("txtNewName");
			((Control)txtNewName).set_Size(new Size(128, 20));
			((Control)txtNewName).set_TabIndex(4);
			((Control)cmdAdd).set_Location(new Point(202, 333));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(100, 26));
			((Control)cmdAdd).set_TabIndex(5);
			((Control)cmdAdd).set_Text("Add this name");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)label1).set_AutoSize(true);
			label1.set_BorderStyle((BorderStyle)2);
			((Control)label1).set_Location(new Point(12, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(323, 106));
			((Control)label1).set_TabIndex(6);
			((Control)label1).set_Text(componentResourceManager.GetString("label1.Text"));
			((Control)cmdSave).set_Location(new Point(202, 389));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(73, 36));
			((Control)cmdSave).set_TabIndex(7);
			((Control)cmdSave).set_Text("Save edits \r\nand quit");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)cmdCancel).set_Location(new Point(293, 390));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(53, 34));
			((Control)cmdCancel).set_TabIndex(8);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(361, 435));
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdSave);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdAdd);
			((Control)this).get_Controls().Add((Control)(object)txtNewName);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Control)this).get_Controls().Add((Control)(object)cmdDown);
			((Control)this).get_Controls().Add((Control)(object)cmdUp);
			((Control)this).get_Controls().Add((Control)(object)List);
			((Control)this).set_Name("PDS_AuthorList");
			((Control)this).set_Text("PDS List of Authors");
			((Form)this).add_Load((EventHandler)PDS_AuthorList_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
