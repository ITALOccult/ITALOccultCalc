using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class DisplayImage : Form
	{
		private string saveName = "";

		private string path = "";

		private IContainer components;

		internal PictureBox picBox;

		private Panel pnlPic;

		private VScrollBar vScrollBar1;

		private HScrollBar hScrollBar1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withImageToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal string SaveName
		{
			set
			{
				saveName = value;
			}
		}

		internal string Path
		{
			set
			{
				path = value;
			}
		}

		public DisplayImage()
		{
			InitializeComponent();
		}

		private void DisplayImage_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			HScrollBar obj = hScrollBar1;
			int left;
			((Control)picBox).set_Left(left = 2);
			((Control)obj).set_Left(left);
			VScrollBar obj2 = vScrollBar1;
			((Control)picBox).set_Top(left = 2);
			((Control)obj2).set_Top(left);
			SetSizes();
		}

		private void DisplayImage_Resize(object sender, EventArgs e)
		{
			SetSizes();
		}

		private void SetSizes()
		{
			((Control)pnlPic).set_Width(((Control)this).get_Width() - 37);
			((Control)pnlPic).set_Height(((Control)this).get_Height() - 88);
			((Control)hScrollBar1).set_Width(((Control)pnlPic).get_Width());
			((Control)vScrollBar1).set_Top(26);
			((Control)vScrollBar1).set_Height(((Control)pnlPic).get_Height());
		}

		private void vScrollBar1_Scroll(object sender, ScrollEventArgs e)
		{
			int num = (int)((float)(((Control)pnlPic).get_Height() - picBox.get_Image().Height) / 450f * (float)((ScrollBar)vScrollBar1).get_Value());
			if (num > 0)
			{
				num = 0;
			}
			((Control)picBox).set_Top(num);
		}

		private void hScrollBar1_Scroll(object sender, ScrollEventArgs e)
		{
			int num = (int)((float)(((Control)pnlPic).get_Width() - picBox.get_Image().Width) / 450f * (float)((ScrollBar)hScrollBar1).get_Value());
			if (num > 0)
			{
				num = 0;
			}
			((Control)picBox).set_Left(num);
		}

		private void DisplayImage_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picBox.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SaveGraphic(picBox.get_Image(), saveName, path);
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
			//IL_01cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d6: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			picBox = new PictureBox();
			pnlPic = new Panel();
			vScrollBar1 = new VScrollBar();
			hScrollBar1 = new HScrollBar();
			menuStrip1 = new MenuStrip();
			withImageToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((ISupportInitialize)picBox).BeginInit();
			((Control)pnlPic).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picBox).set_BackColor(Color.White);
			((Control)picBox).set_Location(new Point(1, 1));
			((Control)picBox).set_Name("picBox");
			((Control)picBox).set_Size(new Size(385, 310));
			picBox.set_TabIndex(0);
			picBox.set_TabStop(false);
			((Control)pnlPic).get_Controls().Add((Control)(object)picBox);
			((Control)pnlPic).set_Location(new Point(2, 26));
			((Control)pnlPic).set_Name("pnlPic");
			((Control)pnlPic).set_Size(new Size(402, 330));
			((Control)pnlPic).set_TabIndex(1);
			((Control)vScrollBar1).set_Anchor((AnchorStyles)10);
			((Control)vScrollBar1).set_Location(new Point(405, 26));
			((ScrollBar)vScrollBar1).set_Maximum(500);
			((Control)vScrollBar1).set_Name("vScrollBar1");
			((Control)vScrollBar1).set_Size(new Size(17, 349));
			((Control)vScrollBar1).set_TabIndex(2);
			((ScrollBar)vScrollBar1).add_Scroll(new ScrollEventHandler(vScrollBar1_Scroll));
			((Control)hScrollBar1).set_Anchor((AnchorStyles)6);
			((Control)hScrollBar1).set_Location(new Point(2, 357));
			((ScrollBar)hScrollBar1).set_Maximum(500);
			((Control)hScrollBar1).set_Name("hScrollBar1");
			((Control)hScrollBar1).set_Size(new Size(402, 18));
			((Control)hScrollBar1).set_TabIndex(3);
			((ScrollBar)hScrollBar1).add_Scroll(new ScrollEventHandler(hScrollBar1_Scroll));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withImageToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(423, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withImageToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withImageToolStripMenuItem).set_Name("withImageToolStripMenuItem");
			((ToolStripItem)withImageToolStripMenuItem).set_Size(new Size(93, 20));
			((ToolStripItem)withImageToolStripMenuItem).set_Text("with Image...  ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy ");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(423, 377));
			((Control)this).get_Controls().Add((Control)(object)hScrollBar1);
			((Control)this).get_Controls().Add((Control)(object)vScrollBar1);
			((Control)this).get_Controls().Add((Control)(object)pnlPic);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DisplayImage");
			((Control)this).set_Text("DisplayImage");
			((Form)this).add_Load((EventHandler)DisplayImage_Load);
			((Control)this).add_Resize((EventHandler)DisplayImage_Resize);
			((ISupportInitialize)picBox).EndInit();
			((Control)pnlPic).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
