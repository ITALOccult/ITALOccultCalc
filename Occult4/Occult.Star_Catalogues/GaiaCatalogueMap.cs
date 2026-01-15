using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class GaiaCatalogueMap : Form
	{
		private int[] NumStars = new int[129960];

		private double LogMaxstars = 1.0;

		internal static bool Plot = true;

		private IContainer components;

		private PictureBox picMap;

		public GaiaCatalogueMap()
		{
			InitializeComponent();
			SetPicSize();
		}

		internal void MapCatalogueCoverage(string GaiaFile)
		{
			try
			{
				GaiaFile = Path.GetFileNameWithoutExtension(GaiaFile);
				((Control)this).set_Text("Whole-sky map of star density - " + GaiaFile);
				int num = 10;
				int num2 = 0;
				using (FileStream input = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + GaiaFile + ".inx", FileMode.Open, FileAccess.Read))
				{
					using BinaryReader binaryReader = new BinaryReader(input);
					for (int i = 0; i < 129960; i++)
					{
						NumStars[i] = binaryReader.ReadInt32();
						if (i > 0)
						{
							num2 = NumStars[i] - NumStars[i - 1];
							if (num2 > num)
							{
								num = num2;
							}
						}
					}
				}
				LogMaxstars = Math.Log10(num);
				PlotMap();
			}
			catch
			{
			}
		}

		private void PlotMap()
		{
			float num = 0f;
			float num2 = 0f;
			float num3 = 1f;
			float num4 = 1f;
			float x = 0f;
			float y = 0f;
			int num5 = 0;
			int num6 = 1;
			Brush[] array = new Brush[14];
			Font font = new Font("Courier New", 8f);
			array[0] = Brushes.Black;
			array[1] = Brushes.Brown;
			array[2] = Brushes.Purple;
			array[3] = Brushes.DarkBlue;
			array[4] = Brushes.DarkRed;
			array[5] = Brushes.DarkGreen;
			array[6] = Brushes.Blue;
			array[7] = Brushes.Red;
			array[8] = Brushes.Green;
			array[9] = Brushes.Gold;
			array[10] = Brushes.Ivory;
			Pen pen = new Pen(Color.LightGray, 0.4f);
			Pen pen2 = new Pen(Color.Azure);
			int width = ((Control)picMap).get_Width();
			int height = ((Control)picMap).get_Height();
			num3 = (float)width / 361f;
			num4 = (float)height / 361f;
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.DarkGray);
			if (Plot)
			{
				for (int num7 = 179; num7 >= -180; num7--)
				{
					double num8 = 1.0 / Math.Cos(((double)(num7 / 2) + 0.5) / (180.0 / Math.PI));
					num2 = (float)(180 - num7) * num4;
					for (int i = 1; i < 361; i++)
					{
						num = (float)(361 - i) * num3;
						num6 = 361 * (179 - num7) + i;
						num5 = (int)(num8 * (double)(NumStars[num6] - NumStars[num6 - 1]));
						if (num5 > 0)
						{
							num5 = (int)(Math.Log10(num5) / LogMaxstars * 12.5);
						}
						if (num5 > 10)
						{
							num5 = 9;
						}
						graphics.FillRectangle(array[num5], num, num2, num3, num4);
					}
				}
				for (int num9 = 80; num9 >= -80; num9 -= 20)
				{
					num2 = (float)(180 - 2 * num9) * num4;
					graphics.DrawLine(pen, 0f, num2, width, num2);
					graphics.DrawString(num9.ToString(), font, Brushes.White, 5f, num2 - 12f);
				}
				for (int j = 2; j < 24; j += 2)
				{
					num = (360.5f - (float)(15 * j)) * num3;
					graphics.DrawLine(pen, num, 0f, num, height);
					graphics.DrawString(j + "h", font, Brushes.White, num + 1f, 4f);
				}
				for (int k = 0; k < 361; k += 5)
				{
					num = (360.5f - (float)k) * num3;
					num2 = (180f - (float)(46.7 * Math.Sin((double)k / (180.0 / Math.PI)))) * num4;
					if (k % 2 > 0)
					{
						graphics.DrawLine(pen2, x, y, num, num2);
					}
					x = num;
					y = num2;
				}
				picMap.set_Image((Image)image);
				graphics.Dispose();
				((Control)picMap).Focus();
			}
			else
			{
				((Control)this).Hide();
			}
		}

		private void GaiaCatalogueMap_Resize(object sender, EventArgs e)
		{
			SetPicSize();
		}

		private void SetPicSize()
		{
			((Control)picMap).set_Top(1);
			((Control)picMap).set_Left(1);
			if (((Control)this).get_Width() > 40)
			{
				((Control)picMap).set_Width(((Control)this).get_Width() - 20);
			}
			if (((Control)this).get_Height() > 40)
			{
				((Control)picMap).set_Height(((Control)this).get_Height() - 44);
			}
			PlotMap();
		}

		private void GaiaCatalogueMap_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			picMap = new PictureBox();
			((ISupportInitialize)picMap).BeginInit();
			((Control)this).SuspendLayout();
			((Control)picMap).set_BackColor(Color.White);
			((Control)picMap).set_Location(new Point(2, 1));
			((Control)picMap).set_Name("picMap");
			((Control)picMap).set_Size(new Size(547, 297));
			picMap.set_TabIndex(0);
			picMap.set_TabStop(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(550, 298));
			((Control)this).get_Controls().Add((Control)(object)picMap);
			((Control)this).set_Name("GaiaCatalogueMap");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("Gaia Catalogue Map");
			((Form)this).add_Load((EventHandler)GaiaCatalogueMap_Load);
			((Control)this).add_Resize((EventHandler)GaiaCatalogueMap_Resize);
			((ISupportInitialize)picMap).EndInit();
			((Control)this).ResumeLayout(false);
		}
	}
}
